#----------------------------------------
# esempio delle 6 scatole (esteso ad n)
#
# GdA, 4/3/2011
#----------------------------------------

# function pausa
pausa <- function() {
  cat ("\n >> Guarda il plot e dai enter per continuare\n")
  scan()
}

delay = 0.15  # ritardo in secondi fra estrazioni (se 0 -> manuale)
delay = 0

np = 100    # nr di palline
ns = np + 1 # nr di scatole  Att: in R parte da 1 !!!

N = 50     # numero di estrazioni

box = sample(1:ns)[1]  # Random box
# box = 2                #  
cat(sprintf("Box: %d\n", box))

prior.unif  <- rep(1/ns, ns)              # prior uniforme
prior.binom <- dbinom(c(0:np), np, 1/2)   # prior binomiale

# usiamo uniforme
p.h <- prior.unif

# likelihoods:
likW <- c(0:np)/np
likB <- 1 - likW

#----------- prima di cominciare le estrazioni:
p.h                     # prob composizioni
p.W <- sum(likW * p.h)  # prob bianca
p.B <- 1- p.W

cat(sprintf("P(B) = %.4f;  P(W) = %.4f\n\n", p.B, p.W))

# estrazioni
extr <- rbinom(N, 1, likW[box])

p.h.M <-  matrix(rep(0, N*ns), c(N, ns))   # matrice dei risultati
prev.W <- rep(0, N)                        # previsione delle bianche

if (length(extr) <= 1000) print(extr)
#---INIZIO --------------------------------------------------
  i=0
  mean  = sum(c(0:np)*p.h)
  sigma = sqrt(sum(c(0:np)^2*p.h) - mean^2)

  ty = ifelse(np < 100, 'p', 'l')  
  plot(c(0:np), p.h, ty=ty, lwd=2, col='blue',
       main=sprintf("Box: %d/%d [H%d/%d]   (extraction: %d)\n Mean=%.2f,  Sigma=%.2f",
       box, ns, box-1, ns-1, i, mean, sigma))
  if(delay) {
      Sys.sleep(0.3)
  } else {
      pausa()
  }
#-------------------------------------------------------

for (i in 1:N) {
  if(extr[i]) {    # se bianca
    cat("\n-> White\n")
    p.h <- likW * p.h / sum(likW * p.h)   # teorema di Bayes
  } else  {
    cat("\n-> Black\n")
    p.h <- likB * p.h / sum(likB * p.h)   # teorema di Bayes
  }
  p.h.M[i,] <- p.h               # prob composizioni
  prev.W[i] <- sum(likW * p.h)   # prob bianca

  if(np <= 20) print(round(p.h,6))
  cat(sprintf("P(B) = %.4f;  P(W) = %.4f\n", 1-prev.W[i], prev.W[i]))

  mean  = sum(c(0:np)*p.h)
  sigma = sqrt(sum(c(0:np)^2*p.h) - mean^2)

  plot(c(0:np), p.h, ty=ty, lwd=2, col='blue',
       main=sprintf("Box: %d/%d [H%d/%d]   (extraction: %d)\n Mean=%.2f,  Sigma=%.2f",
       box, ns, box-1, ns-1, i, mean, sigma))
  if(delay) {
      Sys.sleep(0.3)
  } else {
      pausa()
  }
}

if (length(extr) <= 1000) print(extr)