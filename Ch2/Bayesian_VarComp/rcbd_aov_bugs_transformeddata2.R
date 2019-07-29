model{
  for(i in 1:nb){
    b[i] ~ dnorm(0, tau.b)
  }  
  
  
  for(k in 1:ns){
    mu.bar[k] ~ dnorm(0, 0.00001)
  }
  
  for(i in 1:N){
    intensity[i] ~ dnorm(mu[i], tau)
    mu[i] <- mu.bar[samples[i]] + xi*b[blocks[i]]
  }
  
  xi ~ dnorm(0, (0.007^(-1)))
  tau.b ~ dgamma(0.5, 0.5)
  sigma.b <- abs(xi)/sqrt(tau.b)
  sigma.b.sq <- pow(sigma.b,2)
  
  sigma ~ dunif(0, 1)
  sigma.sq <- pow(sigma,2)
  tau <- 1/sigma.sq
  
  ratio <- sigma.b.sq/sigma.sq
  p.ratio <- step(ratio-1)
}