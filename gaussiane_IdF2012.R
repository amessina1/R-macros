#--------------------------------------------------------------
# Gaussiane sovrapposte, su layout 'minimalista' e possibilitÃ 
# di scrivere su file
#
# GdA 12 Ottobre 2012
#---------------------------------------------------------------

salva=!TRUE   # il punto esclamativo nega TRUE: toglierlo per
              # salvare su file

if(salva) png(filename="gaussiane.png", height=220, width=600, bg="white")

# per falvare su file eps (-> utile per LaTeX), sostituire con, ad es. 
#  postscript(file="gaussiane.eps",onefile=FALSE,horizontal=FALSE,height=2,width=5,pointsize=11)

old.mar = par("mar")     # vecchi margini memorizzati in 'old.mar'
par(mar=c(2.1,4,0.5,1))  # cambia i margini dei plot

x=seq(-10,10,len=100)
plot(x, dnorm(x), ylim=c(-0.05,0.4), ty='l', col='blue',
     xaxt='n',yaxt='n',                 # senza assi
     bty='n', xlab='', ylab='')         # e senza label

abline(h=0, lwd=0.1)                    # asse delle ascisse senza 'fronzoli'
points(x, dnorm(x),col='blue',ty='l')   # altre gaussiane
points(x, dnorm(x, 3,5),col='blue',ty='l')
points(x, dnorm(x, 1, 3),col='red',ty='l')
points(x, dnorm(x, 4, 2),col='green',ty='l')
points(x, dnorm(x, -2, 1.2),col='red',ty='l')
points(x, dnorm(x, -5, 4),col='green',ty='l')
points(0.3,0, pch=19)                  # punto 'sperimentale' 
text(0.3,-0.05, 'x')                   

par(mar=old.mar)                       # rimettiamo a posto i margini

if (salva) dev.off() # si chiude il 'device' di outpout della grafica
                     # salvando coosÃ¬ definitivamente file con il grafico