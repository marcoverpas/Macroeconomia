# Come stimare i coefficienti della funzione di consumo

# Anzitutto dobbiamo scaricare la serie storica dei dati sui consumi e il reddito disponibile
# p.es. da qui: https://ec.europa.eu/eurostat/web/main/data/database

# Una volta scaricate le due serie, le salviamo (in Excel) con l'estensione csv

# Ora carichiamo i dati dal file csv (in questo caso, dal mio Dropbox) ####
data <- read.csv("https://www.dropbox.com/scl/fi/lejngqmu81m0fhe3skubw/Dati_Italia_diff.csv?rlkey=kj6js628wluwhutobve3zim5b&st=latfeg2p&dl=1")

# Ridenominiamo le variabili ####
delta_C <- data$delta_C   # Variazione annua del consumo reale - variazione media dal 2002
delta_Yd <- data$delta_Yd # variazione annua del reddito disponibile - variazione media dal 2002

# Visualizziamo le due variabili di interesse ####
par(mar = c(6, 6, 4, 2))
plot(data$Anno, data$delta_Yd/1000, type="l", col="blue",
     main = "a) Variazioni di reddito disponibile e consumo in Italia (2002-2023)",
     ylab = "Variazioni in miliardi di euro (2010)", xlab = "Anno",
     ylim=range(min(delta_C/1000),max(delta_Yd/1000)))
lines(data$Anno, data$delta_C/1000, col="violet")
abline(h=0)
abline(v=2010)

# Nota: le grandezze sono espresse in termini "reali". L'anno base utilizzato è il 2010 (con prezzi concatenati).

# Facciamo la regressione utilizzando un semplice modello lineare: delta(C) = c0 + c1*delta(Yd) ####
model <- lm(delta_C ~ delta_Yd, data = data)

# Verifichiamo i risultati della regressione ####
summary(model)

# Visualizziamo e salviamo i coefficienti del modello (c0 e c1) ####
print(model$coefficients)
c0 <- coef(model)["(Intercept)"]
c1 <- coef(model)["delta_Yd"]

# Visualizzamo la retta di regressione ####
plot(delta_Yd/1000, delta_C/1000, main = "b) Variazioni di reddito disponibile e consumo in Italia (2002-2023)",
     xlab = "Variazione del consumo \n (miliardi di euro del 2010)",
     ylab = "Variazione del reddito \n (miliardi di euro del 2010)")
abline(model, col = "red1")
abline(h=0)
abline(v=0)

# Calcoliamo la variazione nei consumi prevista sulla base della regressione ####
delta_C_e <- c0 + c1*delta_Yd

# Confrontiamo i consumi previsti dal modello con quelli effettivi ####
plot(data$Anno, delta_C/1000, main = "c) Variazione nei consumi effettivi e previsti",
     xlab = "Anno",
     ylab = "Variazioni in miliardi di euro (2010)")
lines(data$Anno, delta_C_e/1000, col="purple2")
