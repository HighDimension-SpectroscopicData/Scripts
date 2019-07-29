model{
  for(i in 1:n){
    y[i] ~ dnorm(mu[i], tau_i[i])
    mu[i] <- b0 + b1*x[i] #(x[i]-mean(x[]))
    tau_i[i] <- tau*x[i] #(x[i]-mean(x[]))
    sigma2_i[i] <- 1/tau_i[i]
  }
  tau ~ dgamma(0.001, 0.001)
  sigma2 <- 1/tau
  sigma <- sqrt(sigma2)
  b0 ~ dnorm(0.0, 1.0E-4)
  b1 ~ dnorm(0.0, 1.0E-4)
  
  #sigma2_i_mean <- mean(sigma2_i[])
  #sigma2_i_med <- median(sigma2_i[])
  #sigma2_i_sd <- sd(sigma2_i[])

  R2B <- 1-mean(sigma2_i[])/pow(sd(y[]),2)
}