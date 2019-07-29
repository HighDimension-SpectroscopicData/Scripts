model{
  for(i in 1:n){
    y[i] ~ dnorm(mu[i], tau_i[i])
    mu[i] <- b0 + b1*x[i]
           
    sigma2_i[i] <- 0.0001+(b2 + b3/x[i]) #Arbitrary addition of 0.0001 to variance, to avoid it being zero (that affects distribution of y)
    
    tau_i[i] <- (1/(sigma2_i[i]))
    
  }
  
  b0 ~ dnorm(0.0, 1.0E-4)
  b1 ~ dnorm(0.0, 1.0E-4)
  #(a) - Uniform for b2, b3 between 0,1
  #b2 ~ dunif(0.0, 1.0)
  #b3 ~ dunif(0.0, 1.0)
  
  #(b) - Uniform for sqrt(b2) and sqrt(b3) between 0,100 - As given in Gelman
  sqrt.b2 ~ dunif(0.0, 100)
  sqrt.b3 ~ dunif(0.0, 100)
  b2 <- pow(sqrt.b2,2)
  b3 <- pow(sqrt.b3,2)
  
  R2B <- 1-mean(sigma2_i[])/pow(sd(y[]),2)
  
}