fn <- function(x, mu.zero, sig2.zero, alpha=0.05) {

  t<- abs(mean(x)- mu.zero)/sqrt(sig2.zero/length(x))
  seuil <- 2*(1-pnorm(t))
 
  ## Voici une autre methode pour calculer la  meme seuil pour alpha != 0.05
  #seuil <- sqrt(sig2.zero/length(x))*2*pnorm(1-alpha)

  cat("\n")
  
  cat("Test d'une Moyenne égale á", mu.zero, "\n")
  cat("\n")
  cat("Moyenne :", round(mean(x), 2), "\n")
  cat("Variance :", round(var(x), 2), "\n")
  cat("Écart-type :", round(sd(x), 2), "\n")
  
  cat("\n")
  
  cat("Valeur de t :", round(t, 2),"\n")
  cat("La seuil est :", round(seuil, 2),"\n")
  cat("\n")
  cat("+ Seuil observé, test unilateral :", round(seuil*100, 2),"%", "\n")
  cat("+ Seuil observé, test bilateral :", round((seuil/2)*100, 2), "%", "\n")
  
  k =  round(mu.zero+seuil, 2)
  cat("On k = mu.zero + seuil = ", round(mu.zero,2), " + " , round(seuil, 2) ," = ", k)
  cat("\n<!> TEST Moyenne > k : ", mean(x), " > ", k , " = ", mean(x) > k)
  if(mean(x) > k ){
    cat("\n<!>Donc, On rejette l'hypothese null!")
  }else{
    cat("\n<!>Donc, On accepte l'hypothese null")
  }
  
  rg1 = mu.zero + seuil/2
  rg2 = mu.zero - seuil/2
  cat("\n<!>La région critique : ]-inf;",rg2,"[U]", rg1, ";+inf[")
  cat("\n<!>La région d'acceptation : [", rg2, ";", rg1 ,"]")
}


x <- c(2.099, 2.771, 2.306, 2.011,1.236, 1.591, 1.362, 1.868, 3.018, 2.181, 2.513, 2.74, 1.984, 2.279, 2.162, 2.428, 1.525, 1.304, 5.048, 1.714)
fn(x, 2, 1)