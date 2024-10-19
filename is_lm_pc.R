# Il modello IS-LM-PC con regola del banchiere centrale

# Definisci il numero di scenari e di periodi ####
nscen = 2
nperiodi = 60

# Definisci i parametri e le variabili esogene del modello ####
c0 = 12       # Componente autonoma dei consumi
c1 = 0.8      # Propensione marginale al consumo (e all'investimento)
i0 = 6        # Componente autonoma dell'investimento
i1 = 15       # Elasticità dell'investimento al tasso di interesse (preceduta da segno meno)
tau = 0.1     # Aliquota fiscale media sul reddito
Y0 = 81.07143 # Valore di equilibrio della produzione
alpha = 0.005 # Sensibilità della variazione del tasso di inflazione all'output gap
rho = 0.15    # Coefficiente di aggiustamento del tasso di interesse reale corrente al suo livello di equilibrio

# Definisci le variabili endogene del modello ####
Y = matrix(data=Y0, nrow = nscen, ncol = nperiodi )          # Produzione e reddito
Y_n = matrix(data=Y0, nrow = nscen, ncol = nperiodi )        # Produzione e reddito di equilibrio
r = matrix(data=0.02, nrow = nscen, ncol = nperiodi )        # Tasso di interesse reale
r_n = matrix(data=0.02, nrow = nscen, ncol = nperiodi )      # Tasso di interesse reale di equilibrio
C = matrix(data=70.37143, nrow = nscen, ncol = nperiodi )    # Consumo reale
I = matrix(data=5.7, nrow = nscen, ncol = nperiodi )         # Investimento reale
G = matrix(data=5, nrow = nscen, ncol = nperiodi )           # Spesa pubblica (in termini reali)
TT = matrix(data=8.107143, nrow = nscen, ncol = nperiodi )   # Tasse (in termini reali)
pi = matrix(data=0, nrow = nscen, ncol = nperiodi )          # Inflazione

# Definisci le equazioni del modello ####

# Crea il "ciclo" per gli scenari
for (j in 1:nscen){
  
  # Crea il "ciclo" per i periodi
  for (i in 2:nperiodi) {
  
    # Esperimento: shock positivo di politica fiscale (aumento della spesa pubblica)
    if (i>10 && j==2){G[j,i] = 6}
    
    # Crea il "ciclo" per la soluzione simultanea del modello
    for (iter in 1:20) {
    
        # Livello di produzione di equilibrio
        Y_n[j,i] = Y0                             
      
        # Produzione al tempo i
        Y[j,i] = C[j,i] + I[j,i] + G[j,i]     
    
        # Consumo al tempo i
        C[j,i] = c0 + c1*(Y[j,i-1] - TT[j,i])     
        
        # Investimento al tempo i
        I[j,i] = i0 - i1*r[j,i-1]     
        
        # Tasse al tempo i
        TT[j,i] = tau*Y[j,i-1]
        
        # Livello dei prezzi al tempo i (tramite Curva di Phillips)
        pi[j,i] = pi[j,i-1] + alpha*(Y[j,i-1] - Y_n[j,i-1])
    
        # Tasso di interesse di equilibrio (regola della banca centrale, derivata da Y_n = Y)
        r_n[j,i] = (c0 + i0 + G[j,i] - Y_n[j,i]*(1-c1*(1-tau)))/i1  
        
        # Tasso di interesse al tempo i
        r[j,i] = r[j,i-1] + rho*(r_n[j,i] - r[j,i-1])
        
    }
  }
}

# Visualizza i risultati ####
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))

# Produzione
plot(Y[2,2:nperiodi], type = "l", col = "purple2", lwd = 2,
     main = "a) Produzione",
     ylab = "$", xlab = "Periodi",
     ylim=range(80,84))
lines(Y_n[2,2:nperiodi], col = "violet", lty = 2)
legend("topright", c("Corrente", "Di equilibrio"), bty = 'n', cex=1, lty=c(1,2),
       lwd=c(2,1), col = c("purple3","violet"), box.lty=0)

#Output gap
plot(Y[2,2:nperiodi]-Y_n[2,2:nperiodi], type = "l", col = "orange3", lwd = 2,
     main = "b) Output gap",
     ylab = "$", xlab = "Periodi",
     ylim=range(-4,4))
abline(h=0,lty=2)

# Variazione dell'inflazione
plot(pi[2,2:nperiodi]*100-pi[2,1:(nperiodi-1)]*100, type = "l", col = "green3", lwd = 2,
     main = "c) Variazione dell'inflazione",
     ylab = "%", xlab = "Periodi",
     ylim=range(-1.5,1.5))
abline(h=0,lty=2)

# Tasso di interesse
plot(r[2,2:nperiodi]*100, type = "l", col = "cyan3", lwd = 2,
     main = "d) Tasso di interesse reale",
     ylab = "%", xlab = "Periodi",
     ylim=range(0,12) )
lines(r_n[2,2:nperiodi]*100, col = "blue3", lty = 2)
legend("bottomright", c("Corrente", "Di equilibrio"), bty = 'n', cex=1, lty=c(1,2),
       lwd=c(2,1), col = c("cyan3","blue3"), box.lty=0)
