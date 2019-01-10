#----------------------------------------------------------
# alcuni comandi interessanti per gli argomenti trattati
# nella lezione per gli Incontri della Fisica
# (I risultati sono riportati dopo il commento)
#
# GdA, 12 ottobre 2012
#-----------------------------------------------------------

# NOTA: questo non Ã¨ uno script ma una collezione di comandi
#       -> se si segue come script si vede solo l'ultimo plot
#          in quanto quelli precedenti verranno sovrascitti
#       -> eseguire mediante copia/incolla

##### Gaussiane ######################################################
# cumulativa in x=4 della gaussiana avente mu=3 e sigma=1
mu <- 3; sigma <- 1
pnorm(4, mu, sigma)  # 0.8413447

# probabilita' entro n sigma
n <- 1; 
pnorm(mu+n*sigma, mu, sigma) - pnorm(mu-n*sigma, mu, sigma)  # 0.6826895
2*(pnorm(mu+n*sigma, mu, sigma) -0.5)  # equivalente
1 - 2*pnorm(mu-n*sigma, mu, sigma)     # equivalente

# probabilita' oltre n sigma (solo coda a destra)
1 - pnorm(mu+n*sigma, mu, sigma)  # 0.1586553

# Semplice plot di gaussiana e della sua cumulativa
# eseguire un comando 'plot' alla volta
x <- seq(mu-4*sigma, mu+4*sigma, len=100)
plot(x, dnorm(x,mu, sigma), ylab='f(x)', ty='l')
plot(x, pnorm(x,mu, sigma), ylab='F(x)', ty='l')

# i due grafici sovrapposti (+ legenda)
plot(x, dnorm(x,mu, sigma), ylim=c(0, max(1,dnorm(x,mu, sigma))), 
     ylab='f(x) o F(x)', ty='l', col='blue')
points(x, pnorm(x,mu, sigma), ylab='F(x)', ty='l', col='red')
legend(mu - 3*sigma, 0.9, leg=c('f(x)', 'F(x)'),
       text.col=c('blue','red'), lty=1, col=c('blue','red'))

# per generare un numero secondo una gaussiana
rnorm(1, mu, sigma)   # ovviamente il nr cambia di volta in volta
                      # ad es. 1.385637

# per troncarlo a 4 cifre decimali
round(rnorm(1, mu, sigma), 4)  # idem, ad es 3.0114

# calcolare la probabilitÃ  che AVEVA il numero generato
# 1) salvarlo
xr <- round(rnorm(1, mu, sigma), 4)   # idem, ad es 3.4425
# 2) valutare la probabilitÃ  fra xr-0.00005 e xr+0.00005
# 2a) mediante la cumulativa
dxm <- 0.00005
pnorm(xr+dxm, mu, sigma) -  pnorm(xr-dxm, mu, sigma)  # 3.617356e-05
# 2b) f(xr) * (2*dxm)
dnorm(xr, mu, sigma) * 2 * dxm                        # 3.617356e-05

# riga di comando da ripetere piÃ¹ volte (e che dipende da 'nc',
# numero di cifre dopo la virgola, che puÃ² valere al max 6)
nc=4; dxm=10^(-nc)/2; (xr=round(rnorm(1, mu, sigma), nc)); dnorm(xr, mu, sigma) * 2 * dxm

# campione di numeri casuali estratti dalla nosrmale con mu e sigma
nr <- 1000000                # numero di valori da generare 
xr <- rnorm(nr, mu, sigma)   # li mettiamo nel 'vettore' xr
xr[1:100]                    # diamo un'occhiata ai primi cento valori
mean(xr)                     # ce ne calcoliamo media ...
sd(xr)                       # e la deviazione standard
sum(xr)/nr                   # ma potevamo anche calcolare la media 'a mano'
hist(xr, nc=100, col='aquamarine') # istogramma in 100 bin (per i colori,
                                   # in genere qualsiasi colore espresso in
                                   # inglese ha buone chance di funzionare)
h <- hist(xr, nc=100, col='aquamarine') # come sopra, ma ora h contiene
                                        # utili informazioni sull'istogramma
h                          # mostra l'oggetto h, dal quale possiamo valutare
xm <- h$mid[1]             # valore centrale del primo bin
xM <- h$mid[length(h$mid)] # valore centrale dell'ultimo bin
                           # (xm e xM sono soltanto esempi di come
                           # recuperare tali valori, ma non ci serviranno
                           # per il seguito)
dx <- h$mid[2]-h$mid[1]    # ampiezza del bin
                           # Sovrapponiamo infine all'istogramma i punti
                           # indicanti il valore atteso di eventi
                           # in ciascuna cella
points(h$mid, dnorm(h$mid, mu, sigma)*dx*nr, pch=1, cex=0.5, col='blue')
                           # ove, ricordiamo che il nr atteso di eventi
                           # Ã¨ dato dalla probabilitÃ  che un punto
                           # cada nell'intervallino [ovvero f(x)*dx],
                           # moltiplicata per il nr di eventi totali,
                           # ovvero f(x)*dx*nr, da cui, in R
                           # =>  dnorm(h$mid, mu, sigma)*dx*nr

##### chi-quadro ###########################################################
nu <- 20              # nr di gradi di libertÃ 
E.chi2 <- nu          # valore atteso (precisione del valore di chi^2)
s.chi2 <- sqrt(2*nu)  # std dev (incertezza standard di previsione)
m.chi2 <- nu - 2      # valore modale (di massima p.d.f.)

###########  Alternativa -- chi^2(x, nu) -> gamma(x, nu/2, 1/2) ###########
# distribuzione del chi^2
c2 <- seq(0, 10*s.chi2, len=100) # fino a 10*s.chi2 perchÃ© asimmetrica
plot(c2, dchisq(c2, nu), xlab='chi2', ylab='f(chi2)', ty='l')
# (la distribuzione di chi^2 Ã¨ una 'gamma' con di forma (`shape')
# pari a nu/2 e parametro di `rate' pari a 1/2

# p-value per un dato chi2 osservato: P(chi2 >= chi2.obs)
# ad es.:
chi2.obs <- 25
1 - pchisq(chi2.obs, nu)

# plot del p-values in funzione del chi2.obs ('c2o')
# (ordinate in scala log per ovvii motivi)
c2o <- seq(0, 100, len=100)
plot(c2o, 1-pchisq(c2o, nu), ty='l', log='y',
     xlab='chi2 osservato', ylab='p-value')
###########################################################################

###########  Alternativa -- chi^2(x, nu) -> gamma(x, nu/2, 1/2) ###########
# distribuzione del chi^2
c2 <- seq(0, 10*s.chi2, len=100) # fino a 10*s.chi2 perchÃ© asimmetrica
plot(c2, dgamma(c2, nu/2, 1/2), xlab='chi2', ylab='f(chi2)', ty='l')
# (la distribuzione di chi^2 Ã¨ una 'gamma' con di forma (`shape')
# pari a nu/2 e parametro di `rate' pari a 1/2

# p-value per un dato chi2 osservato: P(chi2 >= chi2.obs)
# ad es.:
chi2.obs <- 25
1 - pgamma(chi2.obs, nu/2, 1/2)

# plot del p-values in funzione del chi2.obs ('c2o')
# (ordinate in scala log per ovvii motivi)
c2o <- seq(0, 100, len=100)
plot(c2o, 1-pgamma(c2o, nu/2, 1/2), ty='l', log='y',
     xlab='chi2 osservato', ylab='p-value')
###########################################################################