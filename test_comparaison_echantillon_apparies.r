#  Test de comparaison d'echantillon appariés
#  Test sur l'exemple 6.10 Page 86

test.compApparies = function(X, Y, alpha=0.01){
  Z = Y - X
  Zx = mean(Z)
  n = length(Z)
  Snc = sqrt( ( 1 / (n-1) ) * sum((Z - Zx)**2)   )
  T = sqrt(n-1) * (Zx/Snc)
  seuil = qt(1-alpha, n-1)
  
  cat("\n")
  cat("Test de comparaison d'echantillon appariés \n")
  cat("Z = ", Z, "\n")
  cat("Zx = ", Zx, "\n")
  
  cat("Snc = ", Snc, "\n")
  cat("T(", n-1, ") = ", T, "\n" )
  cat("seuil = ", seuil, "\n")
  
  cat ("<!>Test: T > seuil : ", abs(round(T, 2)), ">", round(seuil, 2), "=", abs(T) > seuil, "\n")
  if(abs(T) > seuil) {
    cat("Donc on rejette l'hypothese null!")
  }else{
    cat("Donc on a accepte l'hypothse null")
  }
  
  
  cat("\n<!>La région critique: ]", round(seuil, 2), "; inf [")
  
}

X = c(173, 166, 150, 158, 160, 160, 183, 165, 142, 155)
Y = c(182, 177, 155, 165, 169, 171, 185, 162, 143, 153)
test.compApparies(X, Y)


