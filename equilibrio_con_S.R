# L'equilibrio macroeconomico e l'evoluzione del risparmio privato e pubblico

# Definisci il numero di periodi (anni) da considerare
periodi = 25

# Definisci il valore dei parametri e delle variabili esogene del modello ####
c0 = 0
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
Spt = matrix(data=0, nrow=1, ncol=periodi)
Sgt = matrix(data=0, nrow=1, ncol=periodi)
St = matrix(data=0, nrow=1, ncol=periodi)

# Definisci un numero sufficiente di iterazioni da far convergere il sistema di equazioni alla soluzione simultanea ####
for (iterazioni in 1:50){
  
  # Definisci la sequenza di periodi ####
    for (t in 2:periodi){
  
      # Introduci spese autonome a partire dal quinto anno successivo a quello di partenza (2) ####
      if(t<7){c0=0
              I_bar=0
              G_bar=0
              T_bar=0}else{c0=0.5
                           I_bar=0.25
                           G_bar=0.25
                           T_bar=0.2}    
      
        # Definisci le equazioni del modello in modo dinamico ####
        Zt[1,t] = Ct[1,t] + It[1,t] + Gt[1,t]  # Domanda di beni (identità)
        Ct[1,t] = c0 + c1*Ydt[1,t-1]           # Consumo (comportamento). Nota il ritardo temporale in Ydt.
        Ydt[1,t] = Yt[1,t] - Tt[1,t]           # Reddito disponibile (identità)
        It[1,t] = I_bar                        # Investimento (comportamento)
        Gt[1,t] = G_bar                        # Spesa pubblica (comportamento)
        Tt[1,t] = T_bar                        # Imposte nette (comportamento)
        Yt[1,t] = Zt[1,t]                      # Produzione e reddito (condizione di equilibrio)
        
        # Aggiungi le equazioni del risparmio
        Spt[1,t] = Ydt[1,t] - Ct[1,t]          # Risparmio privato
        Sgt[1,t] = Tt[1,t] - Gt[1,t]           # Risparmio pubblico (avanzo di bilancio)
        St[1,t] = Spt[1,t] + Sgt[1,t]          # Risparmio aggregato
  }
}

# Calcola il valore della produzione di equilibrio (di stato stazionario) ####
Y_star = (c0+I_bar+G_bar-c1*T_bar)/(1-c1)

# Visualizza i risultati ####

# Evoluzione del PIL
plot(Yt[1,2:periodi], type="l", col="purple2",
     lwd=3, xlab="Anni", ylab="Euro",
     main="a) Produzione: evoluzione nel tempo verso l'equilbrio macroeconomico")
abline(h=Y_star,lty=2)

# Evoluzione dei risparmi
plot(St[1,2:periodi], type="l", col="green3",
     lwd=3, xlab="Anni", ylab="Euro",
     main="b) Evoluzione del risparmio nel tempo",
     ylim=range(min(Sgt[1,2:periodi]),max(Spt[1,2:periodi])))
lines(Spt[1,2:periodi], type="l", col="purple2", lwd=3)
lines(Sgt[1,2:periodi], type="l", col="violet", lwd=3)
abline(h=0)
abline(h=0.25,lty=2)
legend("left",c("Total","Privato","Pubblico"),  bty = 'n', cex=1,
       lty=c(1,1,1), lwd=c(3,3,3),
       col = c("green3","purple2","violet"), box.lty=0)