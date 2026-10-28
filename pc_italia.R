# Stima della curva di Phillips per l'Italia
# Ultima modifica: 28 ottobre 2026
# =========================================================================

# 1. Dati ####
# Le serie possono essere scaricate da Eurostat
# (https://ec.europa.eu/eurostat/web/main/data/database) e salvate in .csv.
#
# Variabili:
#   anno = anno di riferimento
#   u    = tasso di disoccupazione annuale, % (fonte: BCE)
#   pi   = tasso di inflazione annuale, variazione % dell'IPCA (fonte: Eurostat)
#   dpi  = variazione del tasso di inflazione, pi_t - pi_{t-1}

url  <- "https://www.dropbox.com/scl/fi/bc2bjv0cnprj1ltds4g3e/Dati_Italia_PC.csv?rlkey=ny6lpl0htbn11b39iqroams9e&st=j2yuttxx&dl=1"
data <- read.csv(url)

# Controllo rapido della struttura dei dati
str(data)
summary(data)

# Nota: usiamo sempre le colonne del data frame (data$pi, ecc.) invece di
# creare oggetti globali. In particolare, un oggetto chiamato "pi"
# sovrascriverebbe la costante di R pi = 3.14159...


# 2. Grafico delle serie complete ####

# Organizziamo i grafici in una griglia 2x2 e allarghiamo i margini
layout(matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE))
par(mar = c(6, 6, 4, 2))

anni <- range(data$anno)

plot(data$anno, data$u, type = "l", col = "purple2",
     main = paste0("a) Disoccupazione e inflazione in Italia (",
                   anni[1], "-", anni[2], ")"),
     ylab = "%", xlab = "Anno",
     ylim = range(c(data$u, data$pi), na.rm = TRUE))
lines(data$anno, data$pi, col = "cyan3")
abline(h = 0, col = "gray70")
legend("left", c("Disoccupazione", "Inflazione"),
       lty = 1, col = c("purple2", "cyan3"), bty = "n")


# 3. Campione di stima: escludiamo gli anni successivi al 2020 ####
# Selezioniamo per anno invece di eliminare "le ultime 3 righe":
# il codice resta corretto anche se il file viene aggiornato.
pre <- subset(data, anno <= 2020)
anni_pre <- range(pre$anno)

# Controllo di coerenza: dpi deve essere la differenza prima di pi.
# (Per la prima osservazione serve l'inflazione dell'anno precedente.)
all.equal(pre$dpi[-1], diff(pre$pi))


# 4. Modello 1: curva di Phillips originaria ####
#    pi_t = (mu + z) - alpha * u_t
model1 <- lm(pi ~ u, data = pre)
summary(model1)


# 5. Modello 2: curva di Phillips accelerata ####
#    pi_t - pi_{t-1} = (mu + z) - alpha * u_t
model2 <- lm(dpi ~ u, data = pre)
summary(model2)

# Nota: gli R^2 dei due modelli NON sono confrontabili, perché le variabili
# dipendenti sono diverse (pi nel primo, dpi nel secondo).


# 6. Coefficienti nella notazione del manuale ####
# Nella notazione pi = (mu + z) - alpha * u, alpha e' positivo:
# per questo cambiamo segno al coefficiente stimato di u.
mz1    <-  unname(coef(model1)["(Intercept)"])
alpha1 <- -unname(coef(model1)["u"])
r2_1   <-  summary(model1)$adj.r.squared

mz2    <-  unname(coef(model2)["(Intercept)"])
alpha2 <- -unname(coef(model2)["u"])
r2_2   <-  summary(model2)$adj.r.squared

c(mz1 = mz1, alpha1 = alpha1, mz2 = mz2, alpha2 = alpha2)


# 7. Tasso naturale di disoccupazione (solo dal modello 2) ####
# Il tasso naturale e' quello per cui l'inflazione non varia (dpi = 0):
#    0 = (mu + z) - alpha * u_n   =>   u_n = (mu + z) / alpha
# Il calcolo non ha senso sul modello 1: li' (mu + z) / alpha e' solo il
# tasso di disoccupazione associato a un'inflazione nulla.
u_n <- mz2 / alpha2

# Facoltativo: errore standard di u_n con il metodo delta
b    <- coef(model2)                        # b[1] = intercetta, b[2] = pendenza
grad <- c(-1 / b[2], b[1] / b[2]^2)         # derivate di u_n = -b1/b2
se_u_n <- sqrt(drop(t(grad) %*% vcov(model2) %*% grad))

cat(sprintf("Tasso naturale stimato: %.2f%% (e.s. %.2f)\n", u_n, se_u_n))


# Funzione di supporto: numeri a due decimali per le etichette
f2 <- function(x) format(round(x, 2), nsmall = 2)


# 8. Grafico della curva di Phillips originaria ####
plot(pre$u, pre$pi,
     pch = 21, col = "cyan4", bg = "yellow3",
     main = paste0("b) La curva di Phillips originaria in Italia (",
                   anni_pre[1], "-", anni_pre[2], ")"),
     ylab = "Inflazione (%)", xlab = "Disoccupazione (%)")
abline(h = 0, col = "gray70")
abline(model1, col = "purple2", lwd = 2)
legend("topright", bty = "n", text.col = "purple2",
       legend = as.expression(bquote(paste(
         pi[t] == .(f2(mz1)) - .(f2(alpha1)) * u[t],
         ",  adj. ", R^2 == .(f2(r2_1))))))


# 9. Grafico della curva di Phillips accelerata ####
plot(pre$u, pre$dpi,
     pch = 21, col = "red3", bg = "yellow3",
     main = paste0("c) La curva di Phillips accelerata in Italia (",
                   anni_pre[1], "-", anni_pre[2], ")"),
     ylab = "Variazione dell'inflazione (punti %)",
     xlab = "Disoccupazione (%)")
abline(h = 0, col = "gray70")
abline(model2, col = "purple2", lwd = 2)
abline(v = u_n, col = "gray50", lty = 2)   # tasso naturale stimato
legend("topright", bty = "n", text.col = "purple2",
       legend = as.expression(bquote(paste(
         pi[t] - pi[t - 1] == .(f2(mz2)) - .(f2(alpha2)) * u[t],
         ",  adj. ", R^2 == .(f2(r2_2))))))


# 10. Commento ####
# Se prendessimo per buona la curva di Phillips accelerata, il tasso
# naturale di disoccupazione sarebbe pari a (mu + z) / alpha (vedi punto 7).
# Tuttavia la stima e' poco affidabile: la pendenza e' poco significativa,
# l'R^2 e' molto basso e il campione e' breve (circa 24 osservazioni annue),
# per cui l'intervallo di confidenza di u_n e' molto ampio.
