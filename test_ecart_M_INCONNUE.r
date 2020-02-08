###Test sur l’ espérance avec mu inconnue
# Présupposés: X1, X2, ..., Xn ~ N(m, sig)
# Hypothese null: H0: sig = sig.zero

test.ectc = function(x, sig.zero, alpha=0.05){
  n = length(x)
  Sn2 = sum((x-mean(x))**2) / n
  
  T = (n*Sn2) / sig.zero**2
  
  if(sd(x) < sig.zero){
    seuil = 2*pchisq(T, n-1)
  }else{
    seuil = 2*(1-pchisq(T, n-1))
  }
  
  
  cat("Test d'un ecartype égale á", sig.zero, "\n")
  
  cat("\n")
  cat("Moyenne :", round(mean(x), 2), "\n")
  cat("Variance :", round(var(x), 2), "\n")
  cat("Écart-type :", round(sd(x), 2), "\n")
  
  cat("\n")
  cat("\n")
  
  cat("D = ", round(D, 2))
  cat("\nT = ", round(T, 2))
  cat("\nSEUIL = ", round(seuil, 2), "\n\n")
  
  cat("+ Seuil observé, test unilateral :", round(seuil*100, 2),"%", "\n")
  cat("+ Seuil observé, test bilateral :", round((seuil/2)*100, 2), "%", "\n")
  
  k =  ( qchisq(1-alpha, n-1)* sig.zero**2)/n
  cat("On a  k = [ (u_alpha * sig) / n ] = (", round(qchisq(alpha, n-1),2), "*" , round(sig.zero, 2) , "/", n , "=", round(k, 2), ")")
  cat("\n<!> TEST Sn2 > k : ", round(D, 2), " > ", round(k, 2) , " = ", D > k)
  if(D > k ){
    cat("\n<!>Donc, On rejette l'hypothese null!")
  }else{
    cat("\n<!>Donc, On accepte l'hypothese null")
  }
  
  cat("\n<!>La région critique : ] ",round(k,2),"; +inf [")
}

x <- c(2.099, 2.771, 2.306, 2.011,1.236, 1.591, 1.362, 1.868, 3.018, 2.181, 2.513, 2.74, 1.984, 2.279, 2.162, 2.428, 1.525, 1.304, 5.048, 1.714)
test.ectc(x, 1)