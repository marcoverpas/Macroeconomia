# Stima dei coefficienti della funzione di consumo

# 1. Scarichiamo le serie storiche del consumo e del reddito disponibile ####
# (p.es. da qui: https://ec.europa.eu/eurostat/web/main/data/database)

# 2. Salviamo i dati con l'estensione csv utilizzando Excel ####

# 3. Importiamo i dati dal file csv (disponibile su Dropbox) ####
data <- read.csv("https://www.dropbox.com/scl/fi/lejngqmu81m0fhe3skubw/Dati_Italia_diff.csv?rlkey=kj6js628wluwhutobve3zim5b&st=latfeg2p&dl=1")

# 4. Ridenominiamo le due variabili ####
delta_C <- data$delta_C   # Variazione annua del consumo reale - variazione media dal 2002
delta_Yd <- data$delta_Yd # variazione annua del reddito disponibile - variazione media dal 2002

# 5. Visualizziamo graficamente le due variabili di interesse ####
par(mar = c(6, 6, 4, 2))
plot(data$Anno, data$delta_Yd/1000, type="l", col="purple2",
     main = "a) Variazioni di reddito disponibile e consumo in Italia (2002-2023)",
     ylab = "Variazione assoluta \n (miliardi di euro del 2010)", xlab = "Anno",
     ylim=range(min(delta_Yd/1000),max(delta_Yd/1000)))
lines(data$Anno, data$delta_C/1000, col="green3")
abline(h=0)
abline(v=2010)
legend("topleft",c("Reddito disponibile","Consumo"),  bty = 'n', cex=1, lty=c(1,1),
       col = c("purple2","green3"), box.lty=0)

# 6. Eseguiamo la regressione utilizzando un semplice modello lineare: delta(C) = c0 + c1*delta(Yd) ####
model <- lm(delta_C ~ delta_Yd, data = data)

# 7. Verifichiamo i risultati della regressione ####
summary(model)

# 8. Visualizziamo e salviamo i coefficienti del modello (c0 e c1) ####
print(model$coefficients)
c0 <- coef(model)["(Intercept)"]
c1 <- coef(model)["delta_Yd"]

# 9. Visualizzamo la retta di regressione ####
plot(delta_Yd/1000, delta_C/1000,
     pch=21,col="purple2",bg="green",
     main = "b) Variazioni di reddito disponibile e consumo in Italia (2002-2023)",
     xlab = "Variazione del reddito disponibile \n (miliardi di euro del 2010)",
     ylab = "Variazione del consumo \n (miliardi di euro del 2010)",
     ylim = range(min(-120),max(80)))
abline(model, col = "purple2")
abline(h=0)
abline(v=0)

# 10. Calcoliamo la variazione nel consumo prevista dal modello ####
delta_C_e <- c0 + c1*delta_Yd

# 11. Confrontiamo i consumi previsti dal modello con quelli effettivi ####
plot(data$Anno, delta_C/1000,
     pch = 21, col = "purple2",
     main = "c) Variazione nel consumo effettivo e previsto",
     xlab = "Anno",
     ylab = "Variazione assoluta \n (miliardi di euro del 2010)",
     ylim=range(min(delta_C/1000),max(delta_C_e/1000)))
lines(data$Anno, delta_C_e/1000, col="green3")
abline(h=0)
abline(v=2010)
legend("topleft",c("Osservato","Previsto"),  bty = 'n', cex=1,
       pch = c(1,NA), lty=c(NA,1),
       col = c("purple2","green3"), box.lty=0)
