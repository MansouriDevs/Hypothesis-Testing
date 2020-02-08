#  Test de comparaison de deux moyennes
# Nous supposons que sig2.x = sig2.y
#Hypothese null mx = my
#Application sur Exemple 6.8 Page 83

test.compMc = function(X, Y, alpha=.05){
  nx = length(X)
  ny = length(Y)
  
  Snc.x = sum((X - mean(Y))**2)/(nx-1)
  Snc.y = sum((Y - mean(Y))**2)/(ny-1)
  
  Z = (mean(X) - mean(Y)) * sqrt(nx + ny - 2) / sqrt(((nx-1)*Snc.x + (ny-1)*Snc.y)*(1/nx + 1/ny))

  seuil = qt(1-alpha/2, nx+ny-2)

  cat("\n")
  cat("Test de comparaison de deux moyennes mx = my \n")
  
  cat("Z = ", Z, "\n")
  cat("Mx = ", mean(X), "\n")
  cat("My = ", mean(Y), "\n")
  
  cat("seuil = ", seuil, "\n")

  cat ("<!>Test: abs(Z) > seuil : ", abs(round(Z, 2)), ">", round(seuil, 2), "=", abs(Z) > seuil, "\n")
  if(abs(Z) > seuil) {
    cat("Donc on accepte l'hypothese alternative!")
  }else{
    cat("Donc on a accepte l'hypothse null")
  }
  
  cat("\n<!>La région critique: ]", round(seuil, 2), "; inf [")
  
}

X = c(0.521, 2.332, 1.158, -0.656, 2.356, 1.287, 1.514, 1.223, 1.727, 0.866, 0.094)
Y = c(1.915, 2.557, 2.563, 0.918, 1.118, 0.528, 1.538, 2.421, 1.563, 2.940, 3.124, 2.336, 1.475, 2.261, 3.583)

test.compMc(X, Y)


