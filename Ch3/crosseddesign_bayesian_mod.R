model{
  for(i in 1:ndays){
    d[i] ~ dnorm(0, tau.d)
  }  
  
  for(i in 1:nsamples){
    s[i] ~ dnorm(0, tau.s)
  }
  
  for(k in 1:ntype){
    mu.bar[k] ~ dnorm(0, 0.00001)
  }
  
  for(i in 1:N){
    intensity[i] ~ dnorm(mu[i], tau)
    mu[i] <- mu.bar[type[i]] + xi*s[samples[i]] + yi*d[days[i]]
  }
  
  xi ~ dnorm(0, (0.007^(-1)))
  yi ~ dnorm(0, (0.007^(-1)))
  
  tau.d ~ dgamma(0.5, 0.5) 
  sigma.d <- abs(xi)/sqrt(tau.d)
  sigma.d.sq <- pow(sigma.d,2)
  
  #sigma.s ~ dunif(0, 1) # For Sr407, change the limit to (0,2)
  #sigma.s.sq <- pow(sigma.s,2)
  #tau.s <- 1/sigma.s.sq
  
  #Try Inverse-Gamma prior for sigma.s.sq
  #tau.s ~ dgamma(0.001, 0.001)
  #sigma.s.sq <- 1/tau.s
  
  #Try half-t prior for sigma.s.sq or sigma.s.sq ???? Check in Gelman's book/paper
  tau.s ~ dgamma(0.001, 0.001)
  sigma.s <- abs(xi)/sqrt(tau.s)
  sigma.s.sq <- pow(sigma.s,2)
  
  sigma ~ dunif(0, 100)
  sigma.sq <- pow(sigma,2)
  tau <- 1/sigma.sq
  
  ratio <- sigma.d.sq/sigma.sq
  p.ratio <- step(ratio-1)
}