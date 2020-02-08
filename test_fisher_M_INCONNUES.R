# Nous supposons que m1, et m2 sont inconnues
#Reference Page 81
# !!!!! F suit f(nx-1, ny-1) !!!!!

test.fisheri = function(X, Y, alpha=.05){
  nx = length(X)
  ny = length(Y)
  
  sig2.x = sum((X - mean(X))**2)/nx
  sig2.y = sum((Y - mean(Y))**2)/ny
  
  F = (nx*(ny-1)*sig2.x) / (ny*(nx-1)*sig2.y)
  
  t1 = qf(alpha/2, min(c(nx-1, ny-1)), max(c(nx-1, ny-1)))
  t2 = qf(1-alpha/2, min(c(nx-1, ny-1)), max(c(nx-1, ny-1)))
  seuil = qf(1-alpha, min(c(nx-1, ny-1)), max(c(nx-1, ny-1)))
  #Pourquoi une seuil pour le test unilateral ?
  #Consulter Page 22 -> http://r2math.ensfea.fr/wp-content/uploads/sites/8/2011/06/20-5-comparaison.pdf
  
  cat("\n")
  cat("Test de Fisher avex mx et my connues \n")
  
  cat("Nx = ", nx, "\n")
  cat("Ny = ", ny, "\n")
  
  cat("sig2.x = ", sig2.x, "\n")
  cat("sig2.y = ", sig2.y, "\n")

  
  cat("\n")
  cat("\n")
  cat("Valeur de F: ", round(F, 2), "à (",min(c(nx-1, ny-1)),";", max(c(nx-1, ny-1)), ") deg de liberté\n")
  cat("Seuil unilateral: ", round(seuil, 2), "\n")
  cat("t1: ", round(t1, 2), "\n")
  cat("t2: ", round(t2, 2), "\n")
  
  cat("<!> Test de F > seuil : ",  round(F, 2), ">", round(seuil,2), "=", F > seuil, "\n")
  if(F > seuil){
    cat("On rejette l'hypothese null\n")
  }else{
    cat("On accepte l'hypothese null\n")
  }
  
  cat("<!> Region d'acceptance : [ ", round(t1,2), ";", round(t2, 2) , "]")
  
  cat("\n")
  
}

X = c(0.521, 2.332, 1.158, -0.656, 2.356, 1.287, 1.514, 1.223, 1.727, 0.866, 0.094)
Y = c(1.915, 2.557, 2.563, 0.918, 1.118, 0.528, 1.538, 2.421, 1.563, 2.940, 3.124, 2.336, 1.475, 2.261, 3.583)

test.fisheri(X, Y)


