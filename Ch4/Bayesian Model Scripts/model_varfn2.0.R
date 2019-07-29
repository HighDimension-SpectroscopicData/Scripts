model{
  for(i in 1:n){
    y[i] ~ dnorm(mu[i], tau_i[i])
    mu[i] <- b0 + b1*x[i]
    
    sigma2_i[i] <- (b2 + b3/x[i])
    
    tau_i[i] <- 1/sigma2_i[i]
  
  }
 
  b0 ~ dnorm(0.0, 1.0E-4)
  b1 ~ dnorm(0.0, 1.0E-4)
  
  #(a) - Half T-prior for sqrt.b2, sqrt.b3
  #sqrt.b2 ~ dt(0, 0.1, 1)I(0, )
  #b2 <- pow(sqrt.b2, 2)
    
  #sqrt.b3 ~ dt(0, 0.1, 1)I(0, )
  #b3 <- pow(sqrt.b3, 2)
  #p.b3 <- step(b3)
  
  #(b) - Inverse Gamma prior for b2, b3  
  #tau.b2 ~ dgamma(1, 1)
  tau.b2 ~ dgamma(0.001, 0.001)
  b2 <- 1/tau.b2
  sigma.b2 <- sqrt(b2)
  
  tau.b3 ~ dgamma(0.001, 0.001)
  b3 <- 1/tau.b3
  sigma.b3 <- sqrt(b3)
  
  #(c) Mixture Distribution
  #sqrt.b2 ~ dunif(0.0, 100)
  #b2 <- pow(sqrt.b2,2)
  
  #sqrt.b2 ~ dnormmix(c(0.001, 1.6), c(0.0016^(-1), 0.43^(-1)), c(p.b2.1, p.b2.2)) #Al309
  #sqrt.b2 ~ dnormmix(c(0.001, 0.6), c(0.0016^(-1), 0.04^(-1)), c(p.b2.1, p.b2.2)) #Na330
  #sqrt.b2 ~ dnormmix(c(0.001, 1.43), c(0.0016^(-1), 0.43^(-1)), c(p.b2.1, p.b2.2))#I(0, ) #Ba493
  #b2 <- sqrt.b2^2
  #p.b2.1 ~ dunif(0.0, 1.0)
  #p.b2.2 ~ dunif(0.0, 1.0)
  
  #sqrt.b2 ~ dnormmix(c(0.5, 0.5), c(100, 100), c(p.b2.1, p.b2.2))#I(0, )
  #b2 <- sqrt.b2^2
  #p.b2.1 ~ dunif(0.0, 1.0)
  #p.b2.2 ~ dunif(0.0, 1.0)
  
  #sqrt.b3 ~ dnormmix(c(0.001, 1.6), c(0.0016^(-1), 0.43^(-1)), c(p.b3.1, p.b3.2))#I(0, ) #Al309
  #sqrt.b3 ~ dnormmix(c(0.001, 0.6), c(0.0016^(-1), 0.04^(-1)), c(p.b3.1, p.b3.2))#I(0, ) #Na330
  #sqrt.b3 ~ dnormmix(c(0.001, 1.43), c(0.0016^(-1), 0.43^(-1)), c(p.b3.1, p.b3.2))#I(0, ) #Ba493  
  #b3 <- sqrt.b3^2
  #p.b3.1 ~ dunif(0.0, 1.0)
  #p.b3.2 ~ dunif(0.0, 1.0)
  
  #sigma2_i_mean <- mean(sigma2_i[])
  #sigma2_i_med <- median(sigma2_i[])
  #sigma2_i_sd <- sd(sigma2_i[])
  
  #R2B <- 1-mean(sigma2_i[])/pow(sd(y[]),2)
  
  #Cross Validation
#  for (i in 1:n){
#    r[i]<- y[i]-mu[i]
#    sr[i]<- (y[i]-mu[i])*sqrt(tau_i[i])
#  }
}