# Modello VAR per gli Stati Uniti con dati trimestrali dal 1981 (Q1) al 2017 (Q4)

# Carichiamo i pacchetti necessari ####
library(vars)
library(tseries)
library(zoo)

# Importiamo i dati ####
data <- read.csv("https://www.dropbox.com/scl/fi/u8hsns09gaolfr89nz0yl/Dati_VAR_USA.csv?rlkey=n19p28hpn57kp1s5tpqxnnzer&st=rqhworhu&dl=1")

   # Nota: sono 3 serie storiche trimestrali (pil reale, variazione % del deflatore del pil (inflazione) e tasso sui fondi federali)

# Mostriamo le prime righe dei dati per un controllo rapido ####
head(data)

# Cambiamo il formato della colonna "periodo" ####
data$periodo <- as.yearqtr(data$periodo, format = "%Y Q%q")

# Visualizziamo le 3 serie storiche ####
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
plot(data$periodo, data$pil,type="l", col=2, ylab="PIL reale", xlab="Trimestre")
plot(data$periodo, data$p,type="l", col=3, ylab="Tasso di inflazione trimestrale", xlab="Trimestre")
plot(data$periodo, data$r,type="l", col=4, ylab="Tasso sui federal funds", xlab="Trimestre")

# Esprimiamo anche il tasso di inflazione in termini annuali ####
data$p_decimal <- data$p / 100
data$pa <- (1 + data$p_decimal)^4 - 1
data$pa <- data$pa * 100
data <- subset(data, select=-p_decimal)
plot(data$periodo, data$pa,type="l", col="green4", ylab="Tasso di inflazione annuale", xlab="Trimestre")

# Riorganizziamo i dati come serie storiche ####
data_ts <- ts(data[,], start = c(1981, 1), frequency = 4)

# Controlliamo se le serie sono stazionarie ####
adf.test(data_ts[, "pil"])  # ADF test per il PIL
adf.test(data_ts[, "pa"])   # ADF test per il tasso di inflazione
adf.test(data_ts[, "r"])    # ADF test per il tasso sui fondi federali

# Il PIL non è stazionario, per cui prendiamo la differenza prima e ripetiamo il test
pil_diff <- diff(data_ts[, "pil"])
adf.test(pil_diff)  # ADF test per dPIL

# Modifichiamo la tabella dei dati per sostituire il PIL con la sua variazione ####
data_ts_diff <- cbind(pil_diff, data_ts[, "pa"], data_ts[, "r"] )
data_ts_diff <- data_ts_diff[-1, ]
colnames(data_ts_diff) <- c("d_pil", "pa", "r")

# Riordiniamo le variabili in ordine decrescente di esogenità ####
# data_ts_diff <- data_ts_diff[, c("r", "d_pil", "pa")]   #sblocca togliendo il cancelletto

# Selezioniamo i ritardi ("lag") ottimali ####
lag_selection <- VARselect(data_ts_diff, lag.max = 10, type = "both")
print(lag_selection)

   # Nota: i test mostrano che i ritardi ottimali sono 2 o 3

# Procediamo con la stima del modello VAR ####
var_model <- VAR(data_ts_diff, p = 3, type = "both")

   # Nota: "both" significa che consideriamo sia un'intercetta costante che un "trend"   

# Visualizziamo i risultati ####
summary(var_model)

# Analizziamo la correlazione dei residui ####
serial.test(var_model, lags.pt = 12, type = "PT.asymptotic")

   # Nota: il test mostra che è improbabile che i residui siano autocorrelati (buona notizia)

# Creiamo le funzioni di risposta ad impulso (IRF) sul tasso di interesse ####
dpil_result <- irf(var_model, impulse = "r", response = "d_pil", n.ahead = 15)
pa_result <- irf(var_model, impulse = "r", response = "pa", n.ahead = 15)
r_result <- irf(var_model, impulse = "r", response = "r", n.ahead = 15)

# Estraiamo i risultati  ####
dPIL <- dpil_result$irf$r      # Risposta stimata della variazione del PIL
dPIL_L <- dpil_result$Lower$r  # Limite inferiore dell'intervallo di confidenza
dPIL_H <- dpil_result$Upper$r  # Limite superiore dell'intervallo di confidenza

PA <- pa_result$irf$r          # Risposta stimata del tasso di inflazione
PA_L <- pa_result$Lower$r      # Limite inferiore dell'intervallo di confidenza
PA_H <- pa_result$Upper$r      # Limite superiore dell'intervallo di confidenza

R <- r_result$irf$r            # Risposta stimata del tasso di interesse
R_L <- r_result$Lower$r        # Limite inferiore dell'intervallo di confidenza
R_H <- r_result$Upper$r        # Limite superiore dell'intervallo di confidenza

# Visualizziamo le funzioni di risposta ad impulso ####
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))

# IRF della variazione del PIL
plot(dPIL, type="l", lwd = 2, main = "Risposta all'impulso ortogonale da R",
     ylab = "Variazione del PIL", xlab="Trimestre \n Intervallo di confidenza bootstrap al 95%, 100 iterazioni",
     ylim=range(min(dPIL_L),max(dPIL_H)))
lines(dPIL_L, col=2, lty=2)
lines(dPIL_H, col=2, lty=2)
abline(h=0)

# IRF del tasso di inflazione
plot(PA, type="l", lwd = 2, main = "Risposta all'impulso ortogonale da R",
     ylab = "Tasso di inflazione", xlab="Trimestre \n Intervallo di confidenza bootstrap al 95%, 100 iterazioni",
     ylim=range(min(PA_L),max(PA_H)))
lines(PA_L, col=3, lty=2)
lines(PA_H, col=3, lty=2)
abline(h=0)

# IRF del tasso di interesse
plot(R, type="l", lwd = 2, main = "Risposta all'impulso ortogonale da R",
     ylab = "Tasso di interesse", xlab="Trimestre \n Intervallo di confidenza bootstrap al 95%, 100 iterazioni",
     ylim=range(min(R_L),max(R_H)))
lines(R_L, col=4, lty=2)
lines(R_H, col=4, lty=2)
abline(h=0)
   
   # Nota: alternativamente, possiamo limitarci ad utilizzare il comando plot(pil_result), ecc.

# Effettuiamo delle previsioni circa l'andamento futuro delle variabili ####
# Consideriamo 15 trimestri oltre il 2017
# var_forecast <- predict(var_model, n.ahead = 15) #sblocca togliendo il cancelletto
# plot(var_forecast)                               #sblocca togliendo il cancelletto
  
