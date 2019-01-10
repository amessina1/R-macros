#----------------------------------------------------------
# genera 20 punti 'sperimentali' e li confronta con
# il semplice modello y=y0 ed errore gaussiano con sigma
# tutte uguali
#
# GdA, 12 ottobre 2012
#-----------------------------------------------------------

# NOTA: Questo file rappresenta un vero 'script',
# il quale puÃ² essere eseguito, e quindi ripetuto
# a piacere, mediante il comando ('>' rappresenta il 'prompt')
# > source("chi2.R")
#
# vedi informazioni aggiuntive in fondo


salva <- !TRUE  #  "!TRUE" Ã¨ uguale a FALSE: -> togliere '!' per salvare

# il nostro modellino
y0  <- 5
sy  <- 1
np  <- 20           # nr di punti
x   <- seq(1:np)     # ascisse, x da 1 a 20
yth <- rep(y0, np) # y 'teoriche'

if(salva) png(filename="chi2.png", height=500, width=600, bg="white")
old.mar <- par(mar=c(2.1,4,0.5,1)) # cambia i margini

# plot delle aspettative teoriche
plot(x,yth, ylim=c(0,10), ylab='y', pch=19, cex=0.5, col='blue')
# ... a cui aggiungiamo le barre delle previsioni ad un sigma
dx = 0.1 # larghezza degli estremi delle barre
for(i in 1:np) {
  lines(c(x[i],x[i]), c(y0-sy,y0+sy), col='blue')
  lines(c(x[i]-dx,x[i]+dx), c(y0-sy,y0-sy), col='blue')
  lines(c(x[i]-dx,x[i]+dx), c(y0+sy,y0+sy), col='blue')
}
# (barre fatte a mano anche se esistono package per farle
#  -> http://rgm2.lab.nig.ac.jp/RGM2/func.php?rd_id=Hmisc:errbar

# simulazione dei valori misurati
ym <- rnorm(np, y0, sy)
points(x,ym, pch=4, cex=1.2, col='red')

# calcoliamo la probabilitÃ  della configurazione osservata,
# usando per tale calcolo una risoluzione pari a 1/10 di sigma
res <- 0.1
# TRUCCO: per evitare problemi di underflow (nel caso
#         qualcuno volesse giocare con configurazioni di probabilitÃ 
#         estremamente basse) facciamo uso dei logaritmi
# log naturale della prob della configurazione
lp   <- sum(dnorm(ym, y0, sy, log=TRUE)) + np*log(res*sy)
lp10 <- lp/log(10)
p10  <- floor(lp10)  # esponente
dl   <- lp10 - p10
mant <- 10^dl        # mantissa
s    <-  sprintf("P = %.2f E%d",mant,p10)  # stringa da porre sul grafico
cx <- 1.5
if(salva) cx <- 1.2   # dimensione dei caratteri (le font dei file possono
                      # differire da quelle delle finestre grafica: provare)
xt=14; yt=9.4         # coordinate dove porre il testo
text(xt, yt, s, cex=cx, col='red', pos=4)   # fatto!

# adesso aggiungiamo il chi^2 
chi2 <- sum((ym-y0)^2)/sy^2
s = sprintf("chi2 = %.1f", chi2)  # stringa, il resto segue come sopra
xt=14; yt=8.7
text(xt, yt, s, cex=cx, col='red', pos=4)

# aggiungiamo il p-value equivalente, ricordando
# che la distribuzione di chi^2 Ã¨ una 'gamma' di parametri
# nu/2 e 1/2, ove nu in questo caso vale np
pv <- 1 - pgamma(chi2, np/2, 1/2)     # p-value; il resto lo conosciamo...
s  <-  sprintf("p-value = %.4f", pv)
xt=14; yt=8.0
text(xt, yt, s, cex=cx, col='red', pos=4)

par(mar=old.mar)       # risettiamo i parametri
if (salva) dev.off()  # chiudiamo il device e salviamo il file


#------------------------------------------------------------------
# informazioni aggiuntive sull'esecuzione dello script:

# inoltre, se si vuole far eseguire lo script tante volte,
# finchÃ© non si realizza un chi^2 maggiore di, ad es. 30,
# si puÃ² usare il seguente comando ('>' rappresenta il 'prompt')
# > chi2=0; while(chi2 < 30) source("chi2.R")

# a cui si puÃ² aggiungere un piccolo ritardo per poter osservare
# il grafico
#  > chi2=0; while(chi2 < 30) { source("chi2.R"); Sys.sleep(0.5)}

# per intercettare, al contrario, chi^2 molto piccoli:
#  > chi2=Inf; while(chi2 > 7) { source("chi2.R"); Sys.sleep(0.5)}

# Ovviamente, questo sistema non Ã¨ molto efficiente,
# in quanto lo script oltre alla generazione degli eventi
# fa altre cose che fanno perdere tempo
# Per ottimizzarlo, occorrerebbe suddividerlo in una parte che
# simula le 'osservazioni' e calcola il chi^2, seguito
# dalle istruzioni che si occupano della grafica e del calcolo
# del p-value
# => viene lasciato come esercizio

# Inoltre, se veramente si vuole generare una configurazione
# 'anomale', della quale calcolarne chi^2 etc., conviene
# definire in altro modo il vettore dei 'dati osservati' ym
# Ad esempio, si puÃ² aggiungere 'brutalmente' a mano
# un picco aggiungendo a "ym <- rnorm(np, y0, sy)" l'istruzione
# "ym[6:8] <- ym[6:8] + c(2, 4, 2)": a quello che era
# stato generato nei tre canali di interesse viene aggiunto,
# rispettivamente, 2, 4 e 2.