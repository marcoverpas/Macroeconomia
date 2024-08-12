# L'equilibrio macroeconomico: un semplice modello dinamico

# Definisci il numero di periodi (anni) da considerare
periodi = 25

# Definisci il valore dei parametri e delle variabili esogene del modello ####
c0 = 1
c1 = 0.6
I_bar = 0
G_bar = 0
T_bar = 0
Y_star = 0

# Definisci le variabili del modello ####
Zt = matrix(data=0, nrow=1, ncol=periodi)
Yt = matrix(data=0, nrow=1, ncol=periodi)
Ydt = matrix(data=0, nrow=1, ncol=periodi)
Ct = matrix(data=0, nrow=1, ncol=periodi)
It = matrix(data=I_bar, nrow=1, ncol=periodi)
Gt = matrix(data=G_bar, nrow=1, ncol=periodi)
Tt = matrix(data=T_bar, nrow=1, ncol=periodi)

# Definisci un numero sufficiente di iterazioni per convergere alla soluzione simultanea del sistema di equazioni ####
for (iterazioni in 1:50){
  
  # Definisci la sequenza di periodi ####
    for (t in 2:periodi){
  
      # Definisci le equazioni del modello in modo dinamico ####
      
      Zt[1,t] = Ct[1,t] + It[1,t] + Gt[1,t] # Domanda di beni (identità)
      
      Ct[1,t] = c0 + c1*Ydt[1,t-1]          # Consumo (comportamento, endogena)
      
      Ydt[1,t] = Yt[1,t] - Tt[1,t]          # Reddito disponibile (identità)
      
      It[1,t] = I_bar                       # Investimento (comportamento)
      
      Gt[1,t] = G_bar                       # Spesa pubblica (comportamento)
      
      Tt[1,t] = T_bar                       # Imposte nette (comportamento)
      
      Yt[1,t] = Zt[1,t]                     # Produzione e reddito (condizione di equilibrio)
      
  }
}

# Calcola il valore della produzione di equilibrio (di stato stazionario) ####
Y_star = (c0+I_bar+G_bar-c1*T_bar)/(1-c1)

# Visualizza i risultati ####
plot(Yt[1,2:periodi], type="l", col=2, lwd=3, xlab="Anni", ylab="Euro",
     main="Produzione: evoluzione nel tempo verso l'equilbrio macroeconomico")
abline(h=Y_star,lty=2)
