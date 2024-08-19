# Modello VAR per gli Stati Uniti con dati trimestrali dal 1981 (Q1) al 2017 (Q4)

# Carica i pacchetti necessari ####
library(vars)
library(tseries)
library(zoo)

# Carica i dati: pil reale, deflatore del pil (livello dei prezzi) e tasso sui fondi federali ####
data <- read.csv("https://www.dropbox.com/scl/fi/u8hsns09gaolfr89nz0yl/Dati_VAR_USA.csv?rlkey=n19p28hpn57kp1s5tpqxnnzer&st=rqhworhu&dl=1")

# Mostra le prime righe dei dati per un controllo rapido ####
head(data)

# Riorganizza i dati ####
data_ts <- ts(data[,], start = c(1981, 1), frequency = 4)

# Controlla se le serie sono stazionarie ####
adf.test(data_ts[, "gdp"])  # ADF test per il PIL
adf.test(data_ts[, "p"])    # ADF test per il livello dei prezzi
adf.test(data_ts[, "r"])    # ADF test per il tasso sui fondi federali

# Il gdp non è stazionario e forse nemmeno i prezzi, per cui prendi la differenza prima e ripeti il test
gdp_diff <- diff(data_ts[, "gdp"])
adf.test(gdp_diff)  # ADF test per dPIL

p_diff <- diff(data_ts[, "p"])
adf.test(p_diff)  # ADF test per dPIL

# Modifica la tabella dei dati
data_ts_diff <- cbind(gdp_diff, p_diff, data_ts[, "r"] )
data_ts_diff <- data_ts_diff[-1, ]
colnames(data_ts_diff) <- c("d_gdp", "d_p", "r")

# Riordina le variabili in ordine decrescente di esogenità
#data_ts_diff <- data_ts_diff[, c("r", "d_gdp", "d_p")]

# Seleziona i ritardi ("lag") ottimali ####
lag_selection <- VARselect(data_ts_diff, lag.max = 10, type = "const")
print(lag_selection)

   # Nota: i test mostrano che i ritardi ottimali sono 2 o 3

# Procedi con la stima del modello VAR ####
var_model <- VAR(data_ts_diff, p = 2, type = "const")

# Visualizza i risultati ####
summary(var_model)

# Analizza la correlazione dei residui ####
serial.test(var_model, lags.pt = 12, type = "PT.asymptotic")

   # Nota: il test mostra che è improbabile che i residui siano autocorrelati (buona notizia)

# Crea e visualizza le funzioni di risposta ad impulsi (sul tasso di interesse) ####
gdp_result <- irf(var_model, impulse = "r", response = "d_gdp", n.ahead = 10)
plot(gdp_result)

p_result <- irf(var_model, impulse = "r", response = "d_p", n.ahead = 10)
plot(p_result)

r_result <- irf(var_model, impulse = "r", response = "r", n.ahead = 10)
plot(r_result)

# Effettua delle previsioni circa l'andamento futuro delle tre variabili (oltre il 2017) ####
var_forecast <- predict(var_model, n.ahead = 10)
plot(var_forecast)
