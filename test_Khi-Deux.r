## Test de Khi-deux
## Exemple 6.12 Page 90
test.khideux <- function(M, alpha=0.05){
  dsum = 0
  for (i in 1:nrow(M)){
    for (j in 1:ncol(M)){
      dsum = dsum +  (M[i, j]**2)/(sum(M[i,])*sum(M[, j]))
    }
  }
  
  Dn = sum(M) * (dsum-1)
  dl = (nrow(M)-1)*(ncol(M)-1)
  seuil = qchisq(1-alpha, dl)
  
  cat("Test de Khi-deux\n")
  cat("D = ", round(Dn, 2), "\n")
  cat("Seuil = ", round(seuil, 2), "\n")
  
  cat ("<!>Tester D > seuil : ", round(D, 2), ">", round(seuil, 2), "=", D > seuil, "\n")
  
  if(D > seuil){
    cat("Alors on rejette l'hypothese null")
  }else{
    cat("Alors On accepte l'hypothese null")
  }
}


### nrow : nombre de lignes
### ncol : nombre de colones
### byrow: LES DONNEES SONT SAISIES LIGNE APRES LIGNE
A = matrix( c(156, 44, 44, 6), nrow=2, ncol=2, byrow = TRUE)   
test.khideux(A)