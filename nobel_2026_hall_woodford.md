# Box: il Nobel 2026 a Hall e Woodford e il modello nuovo-keynesiano

*Aggiornato al 14 ottobre 2026*

## Il premio

Lunedì 12 ottobre 2026 l'Accademia Reale Svedese delle Scienze ha assegnato il Premio della Banca di Svezia per le scienze economiche in memoria di Alfred Nobel a **Robert E. Hall** e **Michael Woodford**, in parti uguali, "per i fondamenti microeconomici della stabilizzazione macroeconomica" (*for microeconomic foundations of macroeconomic stabilisation*).

Secondo l'Accademia, i due economisti hanno sviluppato modelli macroeconomici che hanno migliorato in modo significativo la nostra capacità di condurre politiche di stabilizzazione efficaci, monetarie e fiscali. Quei modelli sono oggi la base teorica degli strumenti usati da banche centrali e ministeri dell'economia in tutto il mondo.

Il contesto storico aiuta a capire perché. Negli anni Settanta, con le crisi petrolifere, le relazioni statistiche su cui poggiavano i modelli keynesiani tradizionali (come la curva di Phillips) si rivelarono instabili: l'inflazione cresceva insieme alla disoccupazione. Sulla scia della critica di Robert Lucas (Nobel 1995), Hall e Woodford hanno ricostruito i modelli "dal basso", a partire dalle decisioni di famiglie e imprese, che guardano al futuro. Ne segue un'implicazione pratica: comunicando i propri piani, banche centrali e governi influenzano le aspettative e, attraverso queste, l'economia.

Il premio vale 12 milioni di corone svedesi, da dividere in parti uguali, e sarà consegnato a Stoccolma il 10 dicembre, anniversario della morte di Alfred Nobel. Non rientra nei cinque premi originari previsti dal testamento di Nobel: fu istituito dalla banca centrale svedese nel 1968 e viene assegnato dal 1969.

<p align="center">
  <img src="https://www.nobelprize.org/uploads/2026/10/fig_ek_26_3x2.jpg" width="560" alt="Illustrazione del premio 2026: le oscillazioni dell'economia vengono smorzate">
</p>
<p align="center"><sub>La politica di stabilizzazione smorza picchi e cadute del ciclo. Illustrazione: © Johan Jarnestad/The Royal Swedish Academy of Sciences, libera per usi non commerciali.</sub></p>

I ritratti fotografici dei due premiati sono protetti da copyright e non esistono versioni con licenza libera; li trovate negli annunci delle loro università: [Stanford (Hall)](https://news.stanford.edu/stories/2026/10/robert-e-hall-nobel-prize-economic-sciences) e [Columbia (Woodford)](https://news.columbia.edu/news/columbias-michael-woodford-wins-2026-nobel-memorial-prize-economics).

## I premiati

Hall ha mostrato che il consumo dipende dalle aspettative delle famiglie sul reddito futuro; Woodford ha costruito il modello completo a tre equazioni che incorpora quel risultato e lo affianca alla formazione dei prezzi e alla politica monetaria.

| | Robert E. Hall | Michael Woodford |
| --- | --- | --- |
| Nascita | 1943, Palo Alto (California) | 1955, Chicopee (Massachusetts) |
| Affiliazione | Stanford University; Hoover Institution | Columbia University (John Bates Clark Professor) |
| Formazione | BA Berkeley; PhD MIT (1967), relatore Robert Solow | AB Chicago; JD Yale; PhD MIT (1983) |
| Contributo premiato | Se il consumo dipende dalle aspettative, varia solo con informazioni nuove, quindi imprevedibili (1978). Con Mishkin (1982) stima che circa l'80% delle famiglie si comporti così e il resto viva "alla giornata" | *Interest and Prices* (2003): il prototipo del modello NK a tre equazioni. Sulla scia di Wicksell, mostra che la banca centrale agisce tramite il tasso d'interesse, non la quantità di moneta |
| Altro | Ha presieduto per oltre trent'anni il comitato NBER che data le recessioni statunitensi | Con Rotemberg (1997) stima uno dei primi modelli NK microfondati; definisce il proprio approccio "neo-wickselliano" |

Comune a entrambi è l'enfasi sulle aspettative: poiché famiglie e imprese guardano avanti, conta anche ciò che la politica economica annuncia di fare. Per questo, osserva l'Accademia, credibilità e trasparenza sono diventate qualità essenziali di una banca centrale.

## Il modello nuovo-keynesiano in tre equazioni

Al cuore del modello nuovo-keynesiano (NK), che il lavoro dei premiati ha contribuito a costruire, si trovano tre equazioni: una per le famiglie, una per le imprese, una per la banca centrale. Nelle formule, $x$ indica l'output gap, $\pi$ l'inflazione, $i$ il tasso nominale, $r^n$ il tasso reale naturale ed $E_t$ le aspettative formate oggi.

<p align="center">
  <img src="https://www.nobelprize.org/uploads/2026/10/fig2_ek_en_26_copy.jpg" width="620" alt="Modelli keynesiani tradizionali e modelli nuovo-keynesiani microfondati">
</p>
<p align="center"><sub>A sinistra, il modello keynesiano tradizionale: una "scatola" di relazioni statistiche tra prezzi, disoccupazione e PIL. A destra, il modello NK: dentro la scatola, le decisioni di famiglie e imprese. Illustrazione: © Johan Jarnestad/The Royal Swedish Academy of Sciences, libera per usi non commerciali.</sub></p>

### 1. Le famiglie: la "curva IS"

La prima si ricava considerando una famiglia rappresentativa che massimizza la propria utilità, dipendente dal consumo e dal tempo libero, su un orizzonte infinito, dato un vincolo di bilancio. Siccome le condizioni di ottimo sono non lineari, le approssimiamo, log-linearizzandole, in un intorno dello stato stazionario, cioè della situazione in cui, in assenza di shock, le variabili restano costanti. Imponendo poi che tutto ciò che si produce venga consumato, otteniamo una relazione negativa tra output gap (la distanza dell'output dal livello naturale) e scostamento del tasso di interesse reale dal suo livello naturale.

```math
x_t = E_t x_{t+1} - \frac{1}{\sigma}\left(i_t - E_t \pi_{t+1} - r^n_t\right)
```

Ricorda la curva IS, con una differenza: l'output gap di oggi dipende anche da quello atteso per domani.

### 2. Le imprese: la "curva di Phillips"

La seconda si ricava considerando un'impresa in concorrenza monopolistica che massimizza il profitto, ma non può modificare i prezzi quando vuole. Nella versione più usata (alla Calvo), l'impresa ha in ogni periodo solo una certa probabilità di poterlo fare. Log-linearizzando, otteniamo una relazione positiva tra inflazione e output gap, che ricorda la curva di Phillips.

```math
\pi_t = \beta E_t \pi_{t+1} + \kappa x_t
```

Anche qui con una differenza: a contare è l'inflazione attesa oggi per domani, non quella attesa ieri per oggi.

### 3. La banca centrale: addio alla "LM"

La terza descrive la banca centrale, che fissa il tasso di interesse nominale seguendo una regola. La più nota è la regola di Taylor: il tasso sale quando l'inflazione supera l'obiettivo $\pi^*$ e quando l'output gap è positivo.

```math
i_t = r^n + \pi^* + \phi_\pi\left(\pi_t - \pi^*\right) + \phi_x x_t
```

È questa relazione a rimpiazzare la vecchia curva LM. In alternativa, si può ricavare la politica ottimale minimizzando una funzione di perdita che penalizza l'inflazione lontana dall'obiettivo e l'output gap. Ne risulta una relazione tra inflazione e output gap, dalla quale si ricava il tasso da fissare.

### Il ritorno all'equilibrio... o quasi

Se la banca centrale reagisce all'inflazione in eccesso con sufficiente energia, alzando il tasso nominale più che proporzionalmente (il cosiddetto principio di Taylor: nella regola sopra basta $\phi_\pi > 1$), il sistema ha un'unica soluzione non esplosiva. La condizione di non esplosività seleziona questa soluzione, escludendo le traiettorie divergenti. Risultato: ogni shock che allontani inflazione e output dallo stato stazionario viene riassorbito, con il contributo della politica monetaria. Il ritorno, però, non è l'esito di un processo di aggiustamento: discende dall'aver escluso le traiettorie esplosive.

### E la persistenza?

Nella versione base, inoltre, il modello tende a generare una persistenza endogena limitata, spesso insufficiente a riprodurre quella osservata nei dati. Per rimediare si aggiungono frizioni: abitudini nei consumi, salari vischiosi, indicizzazione dei prezzi all'inflazione passata, ecc. A questo punto, ci si diverte a vedere le variabili impiegare più tempo del previsto (incredibile!) per tornare allo stato stazionario.

## Dal modello NK all'IS-LM-PC

Sul piano macroeconomico, il modello NK è un parente stretto dell'IS-LM-PC che trovate nei manuali: le stesse tre relazioni, con la LM sostituita dalla regola del banchiere centrale. Oggi le banche centrali non fissano la quantità di moneta ma il tasso d'interesse; la moneta si adegua passivamente alla domanda. Per questo la LM, nella versione didattica, diventa una retta orizzontale al livello del tasso di policy, e nella versione NK una regola che sposta quel tasso al variare di inflazione e output gap.

| Blocco | IS-LM-PC (manuale) | Modello NK |
| --- | --- | --- |
| Domanda | IS: la produzione cala se il tasso reale sale | IS dinamica: conta anche l'output gap atteso domani |
| Politica monetaria | LM: offerta di moneta data, oppure tasso fissato (LM orizzontale) | Regola di Taylor o politica ottimale: il tasso reagisce a inflazione e output gap |
| Offerta / prezzi | PC: inflazione legata a disoccupazione e aspettative (spesso adattive) | PC nuovo-keynesiana: inflazione legata all'output gap e all'inflazione attesa per domani |
| Aspettative | Spesso guardano al passato | Razionali e rivolte al futuro |

La logica di fondo è la stessa: uno shock sposta domanda o prezzi, la banca centrale muove il tasso, il tasso influenza la domanda, e questa l'inflazione. Cambia il ruolo delle aspettative: nel modello NK, ciò che famiglie e imprese si attendono per domani agisce già oggi.

## Prova tu: il modello in azione

Una versione in forma ridotta del modello IS-LM-PC con regola della banca centrale è disponibile online: **[apri il simulatore IS-LM-PC](https://x52gnt-marco-passarella.shinyapps.io/is_lm_pc_plus/)**. Usalo per verificare il messaggio centrale del box: ogni shock che porta l'inflazione sopra l'obiettivo viene contrastato con un aumento del tasso d'interesse.

**Come funziona il simulatore.** È la versione da manuale (IS-LM-PC) delle tre relazioni, con aspettative che guardano al passato: famiglie e imprese reagiscono a ciò che è accaduto nel periodo precedente, non a ciò che si attendono per domani.

- **IS:** la produzione è la somma di consumi, investimenti e spesa pubblica. I consumi dipendono dal reddito disponibile del periodo precedente; gli investimenti calano quando sale il tasso reale.
- **PC:** l'inflazione dipende dall'output gap del periodo precedente, da un eventuale shock di prezzo e dall'inflazione attesa. Il cursore *grado di ancoraggio delle aspettative* stabilisce come si formano le attese. Con 0 si attende che l'inflazione resti quella del periodo precedente; con 1 si attende che resti all'obiettivo, che nel simulatore è un'inflazione pari a zero (il valore iniziale).
- **Regola della banca centrale (al posto della LM):** la banca centrale fissa direttamente il tasso *reale* e conosce il livello naturale di produzione. Sposta il tasso gradualmente verso il livello che riporta la produzione a quel livello naturale. A questo aggiunge una maggiorazione quando l'inflazione accelera, e una riduzione quando rallenta. Poiché il tasso reale è il nominale meno l'inflazione attesa, alzarlo significa aumentare il tasso nominale più di quanto aumentino le attese di inflazione.

Lo shock scatta intorno al **periodo 11** dei grafici. Lo shock di prezzo è temporaneo e si esaurisce in otto periodi; le variazioni degli altri cursori sono permanenti. Prima di ogni esercizio premi *Ripristina valori iniziali*.

1. **Shock di prezzo.** Porta lo *shock di prezzo* a 0,05, lasciando l'ancoraggio a 0. Che cosa fa la banca centrale (grafico f)? Che cosa succede a output gap e investimenti?
   *Cosa dovresti osservare:* il tasso reale sale da circa il 2% a quasi il 3%, gli investimenti calano di circa il 16% e si apre un output gap negativo di circa 2,5. L'inflazione arriva intorno al 18%. Il tasso comincia a scendere quando l'inflazione sta ancora salendo, ma sempre più lentamente: la regola reagisce all'*accelerazione* dell'inflazione, non al suo livello. Quando l'inflazione rallenta, il tasso scende per qualche periodo poco sotto il 2%, poi torna al 2%. L'inflazione si stabilizza intorno al 12%, ben sopra l'obiettivo.
2. **Lo stesso shock con aspettative ancorate.** Ripeti l'esercizio 1 portando l'ancoraggio a 1, poi a 0,5. Che cosa cambia?
   *Cosa dovresti osservare:* con ancoraggio pieno l'inflazione tocca il 5% e torna vicina a zero in circa sette periodi. Al tasso basta salire fino al 2,4% circa e la recessione è molto più lieve (output gap minimo intorno a −0,5). Con 0,5 si ottiene un caso intermedio: l'inflazione arriva al 7% e poi rientra.
3. **Shock di domanda.** Porta il *consumo autonomo* a 15. Come reagisce la banca centrale? Il tasso torna al livello iniziale? Ripeti poi con l'*investimento autonomo* a 9,7.
   *Cosa dovresti osservare:* con lo shock sui consumi si apre un output gap positivo di circa 5,5 e l'inflazione sale. La banca centrale alza il tasso reale fino al 5% e lo lascia lì: con più domanda autonoma serve un tasso naturale più alto. Il prezzo lo pagano gli investimenti, che si dimezzano circa. Con lo shock sugli investimenti l'output gap arriva a circa 3,7 e il nuovo tasso è il 4%; gli investimenti tornano al livello iniziale, perché il tasso più alto compensa esattamente lo shock. In entrambi i casi la produzione torna al livello naturale, ma senza ancoraggio l'inflazione si ferma in alto (circa 19% e 13%). Prova anche qui ad ancorare le aspettative.
4. **Politica fiscale e offerta.** Riduci l'*aliquota fiscale* a 0,05, poi (dopo il reset) abbassa il *livello naturale di produzione* a 76,1. In che cosa somigliano e in che cosa differiscono i due casi?
   *Cosa dovresti osservare:* in entrambi i casi la produzione supera quella naturale (output gap tra 5 e 6,5), l'inflazione sale e la banca centrale alza il tasso, che si stabilizza al 5,2% nel primo caso e al 3,4% nel secondo. Nel caso fiscale la produzione torna al livello iniziale, ma con più consumi e meno della metà degli investimenti iniziali: la politica monetaria "spiazza" l'espansione fiscale. Nel caso di offerta la produzione scende stabilmente a 76,1, con meno consumi e meno investimenti. È la risposta che, secondo l'Accademia, il modello di Woodford raccomanda dopo uno shock di offerta negativo: alzare i tassi per evitare un'inflazione fuori controllo.

*Domanda per la discussione:* negli esercizi 1, 3 e 4 la banca centrale contrasta l'aumento dell'inflazione e riporta la produzione al livello naturale, ma l'inflazione non torna all'obiettivo se le aspettative non sono ancorate. Per riportarcela, la banca centrale dovrebbe tenere per un po' la produzione *sotto* il livello naturale. Che cosa ci dice questo sul ruolo della credibilità della banca centrale? E come si collega al "ritorno all'equilibrio... o quasi" discusso sopra?

## Per approfondire

**Fonti sul premio**

- [Accademia Reale Svedese delle Scienze: comunicato stampa](https://www.nobelprize.org/prizes/economic-sciences/2026/press-release/)
- [Accademia: spiegazione divulgativa del premio](https://www.nobelprize.org/prizes/economic-sciences/2026/popular-information/) e [background scientifico](https://www.nobelprize.org/prizes/economic-sciences/2026/advanced-information/)
- Schede ufficiali: [Robert E. Hall](https://www.nobelprize.org/prizes/economic-sciences/2026/hall/facts/) e [Michael Woodford](https://www.nobelprize.org/prizes/economic-sciences/2026/woodford/facts/)
- [NBER: annuncio del premio](https://www.nber.org/news/robert-hall-and-michael-woodford-awarded-2026-nobel-prize)

**I lavori citati nel box**

- Blanchard, O., Amighini, A., Giavazzi, F., *Macroeconomia. Una prospettiva europea*, Bologna, il Mulino (il manuale di riferimento per il modello IS-LM-PC).
- Calvo, G. A. (1983), "Staggered Prices in a Utility-Maximizing Framework", *Journal of Monetary Economics*, 12(3), pp. 383-398.
- Galí, J. (2015), *Monetary Policy, Inflation, and the Business Cycle: An Introduction to the New Keynesian Framework and Its Applications*, 2a ed., Princeton, Princeton University Press.
- Hall, R. E. (1978), "Stochastic Implications of the Life Cycle-Permanent Income Hypothesis: Theory and Evidence", *Journal of Political Economy*, 86(6), pp. 971-987.
- Hall, R. E., Mishkin, F. S. (1982), "The Sensitivity of Consumption to Transitory Income: Estimates from Panel Data on Households", *Econometrica*, 50(2), pp. 461-481.
- Lucas, R. E. (1976), "Econometric Policy Evaluation: A Critique", *Carnegie-Rochester Conference Series on Public Policy*, 1, pp. 19-46.
- Romer, D. (2000), "Keynesian Macroeconomics without the LM Curve", *Journal of Economic Perspectives*, 14(2), pp. 149-169.
- Rotemberg, J. J., Woodford, M. (1997), "An Optimization-Based Econometric Framework for the Evaluation of Monetary Policy", *NBER Macroeconomics Annual*, 12, pp. 297-346.
- Taylor, J. B. (1993), "Discretion versus Policy Rules in Practice", *Carnegie-Rochester Conference Series on Public Policy*, 39, pp. 195-214.
- Woodford, M. (2003), *Interest and Prices: Foundations of a Theory of Monetary Policy*, Princeton, Princeton University Press.
