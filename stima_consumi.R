# Come stimare i coefficienti della funzione di consumo

# Anzitutto dobbiamo scaricare la serie storica dei dati sui consumi e il reddito disponibile
# p.es. da qui: https://ec.europa.eu/eurostat/web/main/data/database

# Una volta scaricate le due serie, le salviamo (in Excel) con l'estensione csv

# Ora carichiamo i dati dal file csv (in questo caso, dal mio Dropbox) ####
data <- read.csv("https://www.dropbox.com/scl/fi/l4753ks3g6xn633hy6qhf/Dati_Italia.csv?rlkey=hn6t1vi1052nqj7t3j643avxx&st=8sw8pucy&dl=1")

# Visualizziamo le due variabili di interesse ####
plot(data$Anno, data$Ydt, type="l", col="blue",
     main = "a) Reddito disponibile e consumi in Italia (2021-2023, prezzi costanti)",
     ylab = "Milioni di euro", xlab = "Anno", ylim=range(min(data$Ct),max(data$Ydt)))
lines(data$Anno, data$Ct, col="violet")

# Nota: le grandezze sono espresse in termini "reali". L'anno base utilizzato è il 2010 (con prezzi concatenati).

# Definiamo e ridenominiamo le variabili da utilizzare nella regressione ####
C <- log(data$Ct)   # Logaritmo naturale del consumo reale
Yd <- log(data$Ydt) # Logaritmo naturale del reddito disponibile

# Nota: prendiamo i logaritmi per stabilizzare la varianza delle serie (in gergo, ridurre l'eteroschedasticità)

# Visualizziamo nuovamente le due variabili ####
plot(data$Anno, Yd, type="l", col="blue2",
     main = "b) Reddito disponibile e consumi in Italia (2021-2023, prezzi costanti)",
     ylab = "Log", xlab = "Anno", ylim=range(min(C),max(Yd)))
lines(data$Anno, C, col="violet")

# Facciamo la regressione utilizzando un semplice modello lineare: C = c0 + c1*Yd) ####
model <- lm(C ~ Yd, data = data)

# Verifichiamo i risultati della regressione ####
summary(model)

# Visualizziamo e salviamo i coefficienti del modello (c0 e c1) ####
print(model$coefficients)
c0 <- coef(model)["(Intercept)"]
c1 <- coef(model)["Yd"]

# Visualizzamo la retta di regressione ####
plot(log(data$Yd), log(data$C), main = "c) La relazione tra reddito e consumi: scatterplot con retta di regressione",
     xlab = "Reddito disponibile (log, prezzi costanti)",
     ylab = "Consumo (log, prezzi costanti)")
abline(model, col = "red1")

# Calcoliamo i consumi previsti sulla base della regressione ####
C_e <- c0 + c1*Yd

# Confrontiamo i consumi previsti dal modello con quelli effettivi ####
plot(data$Anno, C, main = "d) Consumi effettivi e previsti",
     xlab = "Anno",
     ylab = "Log")
lines(data$Anno,C_e,col="purple2")
