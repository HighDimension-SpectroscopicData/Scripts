model{
  for(i in 1:n){
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- b0 + b1*x[i]
    #tau_i[i] <- tau*x[i]
    
  }
  tau ~ dgamma(0.001, 0.001)
  sigma2 <- 1/tau
  sigma <- sqrt(sigma2)
  b0 ~ dnorm(0.0, 1.0E-4)
  b1 ~ dnorm(0.0, 1.0E-4)
  R2B <- 1-sigma2/pow(sd(y[]),2)
  
}