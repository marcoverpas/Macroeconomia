# Il modello IS-LM (con LM orizzontale)

# Definisci il numero di scenari
nscen = 2

# Definisci i parametri e le variabili esogene del modello ####
c0 = 12       # Componente autonoma dei consumi
c1 = 0.8      # Propensione marginale al consumo (e all'investimento)
i0 = 6        # Componente autonoma dell'investimento
i1 = 5        # Elasticità dell'investimento al tasso di interesse (preceduta da segno meno)
G = 4         # Spesa pubblica
t = 2         # Tasse (Nota: "T" in R sta per "True")

# Definisci le variabili endogene del modello ####
Y = matrix(data=0, nrow = nscen, ncol = 10 )    # Produzione e reddito per ogni valore del tasso di interesse
Y_t = matrix(data=0, nrow = nscen, ncol = 10 )  # Produzione e reddito di equilibrio dato il tasso di policy fissato dalla BC
i = matrix(data=0, nrow = nscen, ncol = 10 )    # Tasso di interesse
i_t = matrix(data=0, nrow = nscen, ncol = 10 )  # Tasso di interesse di policy fissato dalla BC

# Definisci le equazioni del modello ####

# Crea il "ciclo" per gli scenari
for (j in 1:nscen){

  # Crea il "ciclo" per i valori del tasso di interesse
  for (k in 0:10) {
  
      # Valori assunti dal tasso di interesse
      i[j,k] = k                                           
  
      # Produzione e reddito per ogni valore del tasso di interesse
      Y[j,k] = (c0 + i0 + G - c1*t)/(1-c1) - i1*i[j,k]     

      # Tasso di interesse di policy per ciascuno scenario
      if (j==1){i_t[j,k] = 4}
           else{i_t[j,k] = 6}                              
    
      # Produzione e reddito di equilibrio dato il tasso di policy
      Y_t[j,k] = (c0 + i0 + G - c1*t)/(1-c1) - i1*i_t[j,k] 
    
    }
  }

# Visualizza i risultati ####
layout(matrix(c(1,2), 1, 2, byrow = TRUE))

# Scenario con tasso di policy basso
plot(Y[1,0:10], i[1,0:10], type = "l", col = 2, lwd = 2,
     main = "a) Modello IS-LM: tasso di policy basso",
     ylab = "Tasso di interesse, i", xlab = "Produzione (reddito), Y" )
lines(Y[1,0:10], i_t[1,0:10], col = 4, lwd = 2)
abline(v = Y_t[1,], lty = 3)

# Scenario con tasso di policy alto
plot(Y[2,0:10], i[2,0:10], type = "l", col = 2, lwd = 2,
     main = "b) Modello IS-LM: tasso di policy alto",
     ylab = "Tasso di interesse, i", xlab = "Produzione (reddito), Y" )
lines(Y[2,0:10], i_t[2,0:10], col = 4, lwd = 2)
abline(v = Y_t[2,], lty = 3)
