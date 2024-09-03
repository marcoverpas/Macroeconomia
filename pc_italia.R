# Stima della Curva di Phillips per l'Italia

# 1. Scarichiamo le serie storiche del tasso di disoccupazione e del tasso di inflazione ####
# (p.es. da qui: https://ec.europa.eu/eurostat/web/main/data/database)

# 2. Salviamo i dati con l'estensione csv utilizzando Excel ####

# 3. Importiamo i dati dal file csv (disponibile su Dropbox) ####
data <- read.csv("https://www.dropbox.com/scl/fi/bc2bjv0cnprj1ltds4g3e/Dati_Italia_PC.csv?rlkey=ny6lpl0htbn11b39iqroams9e&st=j2yuttxx&dl=1")

# Nota: u = tasso di disoccupazione annuale (fonte: BCE)
#       pi = indice armonizzato dei prezzi al consumo annuale (fonte: Eurostat)

# 4. Ridenominiamo le variabili ####
u <- data$u       # Tasso di disoccupazione annuale
pi <- data$pi     # Tasso di inflazione annuale
dpi <- data$dpi   # Variazione del tasso di inflazione annuale
Anno <- data$anno # Anno di riferimento

# 5. Visualizziamo graficamente le due variabili di interesse ####

# Prima, però, organizziamo i grafici in uno spazio 2x2 e allarghiamo un po' i margini
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
par(mar = c(6, 6, 4, 2))

# Quindi procediamo con la visualizzazione
plot(Anno, u, type="l", col="purple2",
     main = "a) Disoccupazione e di inflazione in Italia (1997-2023)",
     ylab = "%", xlab = "Anno",
     ylim=range(min(pi),max(u)))
lines(Anno, pi, col="cyan3")
abline(h=0,col="gray70")
legend("left",c("Disoccupazione","Inflazione"),  bty = 'n', cex=1, lty=c(1,1),
       col = c("purple2","cyan3"), box.lty=0)

# 6. Eliminiamo le osservazioni successive alla crisi Covid-19 e ridenominiamo le variabili
data <- head(data,-3)
u <- data$u       # Tasso di disoccupazione annuale
pi <- data$pi     # Tasso di inflazione annuale
dpi <- data$dpi   # Variazione del tasso di inflazione annuale
Anno <- data$anno # Anno di riferimento

# 7. Eseguiamo una prima regressione usando u e pi (modello 1, "PC originaria") ####
model1 <- lm(pi ~ u, data = data)
summary(model1)

# 8. Eseguiamo una seconda regressione usando u e dpi (modello 2, "PC accelerata") ####
model2 <- lm(dpi ~ u, data = data)
summary(model2)

# Nota: il modello 1 sembra migliore del secondo (anche se nessuno dei due sembra catturare bene la dinamica dei prezzi).

# 8. Visualizziamo e salviamo i coefficienti del modello 1 ####
print(model1$coefficients)
mz1 <- coef(model1)["(Intercept)"]
alpha1 <- coef(model1)["u"]

# 9. Visualizziamo e salviamo i coefficienti del modello 2 ####
print(model2$coefficients)
mz2 <- coef(model2)["(Intercept)"]
alpha2 <- coef(model2)["u"]

# 9. Eseguiamo lo "scatterplot" della PC originaria aggiungendo la retta di regressione
plot(u, pi,
     pch=21,col="cyan4",bg="yellow3",
     main = "b) La curva di Phillips originaria in Italia (1997-2020)",
     ylab = "Inflazione (%)", xlab = "Disoccupazione (%)",
     ylim=range(min(pi),max(pi)))
abline(h=0, col="gray70")
abline(model1, col="purple2", lwd=2)
text(x = 8.2, y = 1.1, labels = expression(paste(pi[t] == 4.06 - 0.24 * u[t], ",  adj. ", R^2==0.16)), cex = 1.2, col = "purple2")

# 10. Eseguiamo lo "scatterplot" della PC accelerata aggiungendo la retta di regressione
plot(u, dpi,
     pch=21,col="red3",bg="yellow3",
     main = "c) La curva di Phillips accelerata in Italia (1997-2020)",
     ylab = "Variazione dell'inflazione (%)", xlab = "Disoccupazione (%)",
     ylim=range(min(dpi),max(dpi)))
abline(h=0, col="gray70")
abline(model2, col="purple2", lwd=2)
text(x = 8.6, y = -1.5, labels = expression(paste(pi[t] - pi[t-1] == 1.06 - 1.12 * u[t], ",  adj. ", R^2==0.01)), cex = 1.2, col = "purple2")

# Nota: se prendessimo per buono il modello con PC accelerata, allora il tasso di 
#       disoccupazione di equilibrio sarebbe pari a: 1.0611/0.1255 = 8.45%.
#       Tuttavia, la significatività dei coefficienti e il coefficiente di determinazione
#       sono davvero bassi!
