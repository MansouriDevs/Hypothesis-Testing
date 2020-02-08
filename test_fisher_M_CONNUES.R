# Nous supposons que m1, et m2 sont connues
#Reference Remarque 6.4 Page 82
# !!!!! F suit f(nx, ny) !!!!!

test.fisherc = function(X, Y, mx, my, alpha=.05){
  nx = length(X)
  ny = length(Y)
  
  sig2.x = sum((X - mx)**2)/nx
  sig2.y = sum((Y - my)**2)/ny
  
  F = (sig2.x/var(X)) / (sig2.y/var(Y))
  
  t1 = qf(alpha/2, min(c(nx, ny)), max(c(nx, ny)))
  t2 = qf(1-alpha/2, min(c(nx, ny)), max(c(nx, ny)))
  seuil = qf(1-alpha, min(c(nx, ny)), max(c(nx, ny)))
  #Pourquoi une seuil pour le test unilateral ?
  #Consulter http://r2math.ensfea.fr/wp-content/uploads/sites/8/2011/06/20-5-comparaison.pdf
  
  cat("\n")
  cat("Test de Fisher avex mx et my connues \n")
  
  cat("Nx = ", nx, "\n")
  cat("Ny = ", ny, "\n")
  
  cat("sig2.x = ", sig2.x, "\n")
  cat("sig2.y = ", sig2.y, "\n")
  
  cat("Mx = ", mx, "\n")
  cat("My = ", my, "\n")
  
  cat("\n")
  cat("\n")
  cat("Valeur de F: ", round(F, 2), "à (",min(c(nx, ny)),";", max(c(nx, ny)), ") deg de liberte\n")
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

test.fisherc(X, Y, 3, 4)


