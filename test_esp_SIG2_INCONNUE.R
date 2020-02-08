###Test sur l’ espérance avec sig2 inconnue
# Présupposés: X1, X2, ..., Xn ~ N(mu.zero, sig2)
# Hypothese null: H0: mu = mu.zero

test.espi <- function(x, mu.zero, alpha=0.05) {
  Snc = sd(x) * (sqrt(length(x)) / sqrt(length(x)-1))
  t <- abs( (mean(x) - mu.zero) * sqrt(length(x)) )/Snc
  
  k <- qt(1-alpha, dl <- length(x)-1);
  cat("\n")
  cat("Test d'une Moyenne égale á", mu.zero, "\n")
  cat("\n")
  cat("Moyenne :", round(mean(x), 2), "\n")
  cat("Snc :", round(Snc, 4), "\n")
  cat("Écart-type :", round(sd(x), 9), "\n")

  
  cat("\n")
  cat("Valeur de t :", round(t, 2), "avec", dl, "degree de liberté", "\n")
  
  k =  qt(1-alpha, 19);
  cat("On k = ", round(k, 2))
  cat("\n<!> TEST de t > k : ", mean(x), " > ", k , " = ", mean(x) > k)
  if(t > k ){
    cat("\n<!>Donc, On rejette l'hypothese null!")
  }else{
    cat("\n<!>Donc, On accepte l'hypothese null")
  }
}
x <- c(2.099, 2.771, 2.306, 2.011,1.236, 1.591, 1.362, 1.868, 3.018, 2.181, 2.513, 2.74, 1.984, 2.279, 2.162, 2.428, 1.525, 1.304, 5.048, 1.714)
test.espi(x, 2)