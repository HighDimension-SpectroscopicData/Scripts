model{
  for(i in 1:nb){
    b[i] ~ dnorm(0, tau.b)
  }  
  
  
  for(k in 1:ns){
    mu.bar[k] ~ dnorm(0, 0.00001)
  }
  
  for(i in 1:N){
    intensity[i] ~ dnorm(mu[i], tau)
    mu[i] <- mu.bar[samples[i]] + b[blocks[i]]
  }
  
  ## Absolute T-dist as prior
  #t.sigma ~ dt(0, 0.1, 1)
  #sigma <- abs(t.sigma)
  #t.sigma.b ~ dt(0, 0.1, 1)
  #sigma.b <- abs(t.sigma.b)
  #t.sigma.r ~ dt(0, 0.1, 1)
  #sigma.r <- abs(t.sigma.r)
  
  ## Uniform Prior
#  sigma ~ dunif(0, 1)
#  sigma.b ~ dunif(0, 1)
  #sigma.r ~ dunif(0, 55000)
  
#  sigma.sq <- pow(sigma,2)
#  sigma.b.sq <- pow(sigma.b,2)
  #sigma.r.sq <- pow(sigma.r,2)
  
#  tau <- 1/sigma.sq
#  tau.b <- 1/sigma.b.sq
  #tau.r <- 1/sigma.r.sq
  
  #mu.bar ~ dnorm(0.0, 1.0E-10)	
  
  ## Inverse Gamma Prior
#  tau ~ dgamma(0.001, 0.001)
#  tau.b ~ dgamma(0.00001, 0.00001)
  #tau ~ dgamma(0.001, 0.001)
  #tau.b ~ dgamma(0.001, 0.001)
  #tau.r ~ dgamma(0.001, 0.001)
  
#  sigma.b.sq <- 1/tau.b
  #sigma.r.sq <- 1/tau.r
#  sigma.sq <- 1/tau
}