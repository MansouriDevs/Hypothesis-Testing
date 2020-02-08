library(ggplot2)

t <- c(2.099, 2.771, 2.306, 2.011,1.236, 1.591, 1.362, 1.868, 3.018, 2.181, 2.513, 2.74, 1.984, 2.279, 2.162, 2.428, 1.525, 1.304, 5.048, 1.714)
t <- sort(t)
#z <- seq(-4,4, by = .01)
Density <- dnorm(z)
criteria <- factor(rep("retain", length(z)), levels=c("retain", "reject"))
criteria[which(z < qnorm(.05))] <- "reject"
qplot(z,Density, geom=c("path","area"), fill=criteria) +
  scale_fill_manual(values=c("grey40", "re