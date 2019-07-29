model{
  for(i in 1:n){
    y[i] ~ dnorm(b0, tau)
  }
  tau ~ dgamma(0.001, 0.001)
  sigma2 <- 1/tau
  sigma <- sqrt(sigma2)
  b0 ~ dnorm(0.0, 1.0E-4)
  
  R2B <- 1-(sigma2)/pow(sd(y[]),2)
}