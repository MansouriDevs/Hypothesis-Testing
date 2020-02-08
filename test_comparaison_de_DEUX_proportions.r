#  Test de comparaison de deux proportions
#Hypothese null p1 = p2
#Application sur Exemple 6.9 Page 84

test.compProp = function(n1, n2, f1, f2, Y, alpha=.05){
  F = (n1*f1 + n2*f2)/(n1+n2)
  
  Z = (f1 - f2) / sqrt(F*(1-F)*(1/n1 + 1/n2))
  
  seuil = qnorm(1-alpha/2)
  
  cat("\n")
  cat("Test de comparaison de deux proportions mx = my \n")
  
  cat("Z = ", Z, "\n")
  cat("F = ", sqrt(F*(1-F)*(1/n1 + 1/n2)), "\n")

  cat("seuil = ", seuil, "\n")
  
  cat ("<!>Test: abs(Z) > seuil : ", abs(round(Z, 2)), ">", round(seuil, 2), "=", abs(Z) > seuil, "\n")
  if(abs(Z) > seuil) {
    cat("Donc on rejette l'hypothese null!")
  }else{
    cat("Donc on a accepte l'hypothse null")
  }
  
  
  cat("\n<!>La région critique: ]", round(seuil, 2), "; inf [")
  
}

test.compProp(118, 210, 0.4, 0.79)


