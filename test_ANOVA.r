## Test d'ANOVA
## Exemple 6.11 Page 88

test.anova <- function(X, Y){
  nx = length(X)
  ny = length(Y)
  n = nx + ny
  
  Z = append(X, Y)
  
  S2.r =(1/(n-2)) * ((nx-1)*var(X) + (ny-1)*var(Y))
  S2.f = sum(nx*(mean(X)-mean(Z))**2) + sum(ny*(mean(Y)-mean(Z))**2)
  
  F = S2.f/S2.r
  
  cat("S2.r = ", round(S2.r, 2),  "\n")
  cat("S2.f = ", round(S2.f, 2), "\n")
  cat("F = ", round(F, 2), "\n")
  cat(">>> F suit la loi de fisher (1,", n-2, ") alors: \n")
  f1  <- round(qf(1-0.05, 1, n-2), 2)
  f2  <- round(qf(1-0.025,1, n-2), 2)
  f3  <- round(qf(1-0.001,1, n-2), 2)
  
  cat("<Test> Quand F > f_alpha alors l'influence est significative:\n")
  cat("1. Pour alpha = 5%, f_0.05 = ", f1, "\n")
  cat("2. Pour alpha = 2.5%, f_0.025 = ", f2, "\n")
  cat("3. Pour alpha = 1%, f_0.001 = ", f3, "\n")
}

X = c(1.5, 1.6, 1.4, 2.9, 2.2, 1.8, 2.7, 1.9, 2.2, 2.8, 2.1, 1.8, 3.7, 1.8, 2.1)
Y = c(4.2, 5.5, 4.6, 5.4, 3.9, 5.4, 2.7, 3.9, 4.1, 4.1, 4.6, 3.9, 3.5)
test.anova(X, Y)
