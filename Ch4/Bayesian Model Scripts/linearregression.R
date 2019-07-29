library(rjags)
library(R2jags)
load.module("mix")
#install.packages("mcmcplots")
library(mcmcplots)
#install.packages("MCMCvis")
library(MCMCvis)

setwd("~/Documents/Anjali Gupta/University of Auckland/Research/ESR/2.1Experiment-PostGrad")

data_size2 <- read.csv('Data_Transformed.csv', header=TRUE)

setwd("~/Documents/Anjali Gupta/University of Auckland/Research/ESR/2.1Experiment-PostGrad/Bayesian Model")
#Model with Variable Variance, Size as the Explanatory Variable
for(j in 2:ncol(data_size2)){
  element <- colnames(data_size2)[j]
  
  bugsData <- list(n = nrow(data_size2), x=data_size2[,1], y=data_size2[,j])
  bugsInits <- list(b0=0, b1=0, tau=1)
  bugsFile = "model.R"
  parameters = c("b0", "b1", "R2B", "sigma2_i") #"sigma2_i_med", "sigma2_i_mean", "sigma2_i_sd", "sigma", "sigma2", 
  
  mod_samples <- paste(element, "samples", sep=".")
  mod_dic <- paste(element, "dic", sep=".")
  mod_hpd <- paste(element, "hpd", sep=".")
  mod_trace <- paste(element, "traceplot", "png", sep=".")
  mod_gelman <- paste(element, "gelman", "png", sep=".")
  mod_sigma2i <- paste(element, "sigma2i", sep=".")
  mod_sigma2i_hpd <- paste(element, "sigma2i", "hpd", sep=".")
  
  sim = jags.model(file=bugsFile, data=bugsData, inits=bugsInits, n.chains = 2)
  update(sim, 50000)
  samples_mod = coda.samples(sim, parameters, 50000, thin = 50)
  summary_mod <- summary(samples_mod)
  #Combining multiple chains in one for HPD Interval
  samples_mod_combined <- mcmc(do.call(rbind, samples_mod)) #Total 2000 values for each parameter
  #Get sigma2_i values together to get summary statistics and compute HPD interval of the same
  sigma2_i_val <- unname(summary_mod$statistics[4:40,"Mean"])
  sigma2_i_val_mcmc <- mcmc(sigma2_i_val)
  #varnames(samples_mod[[1]])
  #varnames(samples_mod[[2]])
  
  assign(mod_samples, print(summary(samples_mod)))
  assign(mod_sigma2i, print(summary(sigma2_i_val_mcmc)))
  assign(mod_dic, print(dic.samples(sim, 50000, "pD")))
  
  #sigma2_i[] = sigma2/x[]
  #mean(summary_mod$statistics[6:42,"Mean"]) #Match with sigma2_i_mean
  #sd(summary_mod$statistics[6:42,"Mean"]) #Match with sigma2_i_sd
  #median(summary_mod$statistics[6:42,"Mean"])
  assign(mod_hpd, print(HPDinterval(samples_mod_combined)))
  assign(mod_sigma2i_hpd, print(HPDinterval(sigma2_i_val_mcmc)))
  
  # Convergence Diagnostics
  png(mod_trace)
  par(mfrow=c(2,2))
  plot(samples_mod[,c('b0','b1')]) #,'sigma2_i'
  #plot(samples_mod[,'sigma2_i[2]'])
  dev.off()
  
  png(mod_gelman)
  gelman.plot(samples_mod[,c('b0','b1')])
  dev.off()
  
  #rmeanplot(samples_mod, parms=parameters)
  #autocorr.plot(samples_mod)
  #rejectionRate(samples_mod)
}

setwd("~/Documents/Anjali Gupta/University of Auckland/Research/ESR/2.1Experiment-PostGrad/Bayesian Model")
#Model with constant Variance
for(j in 2:ncol(data_size2)){
  element <- colnames(data_size2)[j]
  
  bugsData <- list(n = nrow(data_size2), x=data_size2[,1], y=data_size2[,j])
  bugsInits <- list(b0=0, b1=0, tau=1)
  bugsFile = "model_withconstantVar.R"
  
  parameters = c("sigma", "sigma2", "b0", "b1", "R2B")
  
  mod_samples <- paste(element, "samples", sep=".")
  mod_dic <- paste(element, "dic", sep=".")
  mod_trace <- paste(element, "traceplot", "png", sep=".")
  mod_gelman <- paste(element, "gelman", "png", sep=".")
  
  sim = jags.model(file=bugsFile, data=bugsData, inits=bugsInits, n.chains = 2)
  update(sim, 50000)
  samples_mod = coda.samples(sim, parameters, 50000, thin = 50)
  
  assign(mod_samples, print(summary(samples_mod)))
  assign(mod_dic, print(dic.samples(sim, 50000, "pD")))
  
  # Convergence Diagnostics
  png(mod_trace)
  par(mfrow=c(3,2))
  plot(samples_mod[,c('b0','b1','sigma2')])
  dev.off()
  
  png(mod_gelman)
  gelman.plot(samples_mod)
  dev.off()
  
  #rmeanplot(samples_mod, parms=parameters)
  #autocorr.plot(samples_mod)
  #rejectionRate(samples_mod)
}

setwd("~/Documents/Anjali Gupta/University of Auckland/Research/ESR/2.1Experiment-PostGrad/Bayesian Model")
#Model with no predictor and constant Variance
for(j in 2:ncol(data_size2)){
  element <- colnames(data_size2)[j]
  
  bugsData <- list(n = nrow(data_size2), y=data_size2[,j])
  bugsInits <- list(b0=0, tau=1)
  bugsFile = "model_withnopredictor.R"
  
  parameters = c("sigma", "sigma2", "b0", "R2B")
  
  mod_samples <- paste(element, "samples", sep=".")
  mod_dic <- paste(element, "dic", sep=".")
  mod_trace <- paste(element, "traceplot", "png", sep=".")
  mod_gelman <- paste(element, "gelman", "png", sep=".")
  mod_hpd <- paste(element, "hpd", sep=".")
  
  sim = jags.model(file=bugsFile, data=bugsData, inits=bugsInits, n.chains = 2)
  update(sim, 50000)
  samples_mod = coda.samples(sim, parameters, 50000, thin = 50)
  summary_mod <- summary(samples_mod)
  #Combining out of multiple chains in one for HPD Interval
  samples_mod_combined <- mcmc(do.call(rbind, samples_mod))
  
  assign(mod_samples, print(summary(samples_mod)))
  assign(mod_dic, print(dic.samples(sim, 50000, "pD")))
  assign(mod_hpd, print(HPDinterval(samples_mod_combined)))
  
  # Convergence Diagnostics
  png(mod_trace)
  par(mfrow=c(2,2))
  plot(samples_mod[,c('b0','sigma2')])
  dev.off()
  
  png(mod_gelman)
  gelman.plot(samples_mod)
  dev.off()
  
  #rmeanplot(samples_mod, parms=parameters)
  #autocorr.plot(samples_mod)
  #rejectionRate(samples_mod)
}

#Model with Variance Function 1.0
setwd("~/Documents/Anjali Gupta/University of Auckland/Research/ESR/2.1Experiment-PostGrad/Bayesian Model")
#Model with Variable Variance dependent on fragment size - linear model, Size as the Explanatory Variable - V1
for(j in 2:ncol(data_size2)){
  element <- colnames(data_size2)[j]
  
  bugsData <- list(n = nrow(data_size2), x=data_size2[,1], y=data_size2[,j])
  
  bugsInits <- list(b0=0, b1=0, sqrt.b2=0,sqrt.b3=0)#, tau=1,b2=0, b3=0
  bugsFile = "model_varfn1.0.R"
  parameters = c("R2B", "b0", "b1", "sigma2_i", "b2", "b3") #"sigma2_i_med", "sigma2_i_mean", "sigma2_i_sd", "sigma", "sigma2",  "R2B",
  
  mod_samples <- paste(element, "samples", sep=".")
  mod_dic <- paste(element, "dic", sep=".")
  mod_hpd <- paste(element, "hpd", sep=".")
  mod_trace <- paste(element, "traceplot", "png", sep=".")
  mod_gelman <- paste(element, "gelman", "png", sep=".")
  mod_sigma2i <- paste(element, "sigma2i", sep=".")
  mod_sigma2i_hpd <- paste(element, "sigma2i", "hpd", sep=".")
  
  sim = jags.model(file=bugsFile, data=bugsData, inits=bugsInits, n.chains = 2)
  update(sim, 50000)
  samples_mod = coda.samples(sim, parameters, 50000, thin = 50)
  summary_mod <- summary(samples_mod)
  #Combining multiple chains in one for HPD Interval
  samples_mod_combined <- mcmc(do.call(rbind, samples_mod)) #Total 2000 values for each parameter
  #Get sigma2_i values together to get summary statistics and compute HPD interval of the same
  sigma2_i_val <- unname(summary_mod$statistics[4:40,"Mean"])
  sigma2_i_val_mcmc <- mcmc(sigma2_i_val)
  #varnames(samples_mod[[1]])
  #varnames(samples_mod[[2]])
  
  assign(mod_samples, print(summary(samples_mod)))
  assign(mod_sigma2i, print(summary(sigma2_i_val_mcmc)))
  assign(mod_dic, print(dic.samples(sim, 50000, "pD")))
  
  #sigma2_i[] = sigma2/x[]
  #mean(summary_mod$statistics[6:42,"Mean"]) #Match with sigma2_i_mean
  #sd(summary_mod$statistics[6:42,"Mean"]) #Match with sigma2_i_sd
  #median(summary_mod$statistics[6:42,"Mean"])
  assign(mod_hpd, print(HPDinterval(samples_mod_combined)))
  assign(mod_sigma2i_hpd, print(HPDinterval(sigma2_i_val_mcmc)))
  
  # Convergence Diagnostics
  png(mod_trace)
  par(mfrow=c(4,2))
  plot(samples_mod[,c('b0','b1', 'b2', 'b3')]) #,'sigma2_i'
  #plot(samples_mod[,'sigma2_i[2]'])
  dev.off()
  
  png(mod_gelman)
  gelman.plot(samples_mod[,c('b0','b1','b2','b3')])
  dev.off()
  
  #rmeanplot(samples_mod, parms=parameters)
  #autocorr.plot(samples_mod)
  #rejectionRate(samples_mod)
}

#Model with Variance Function 2.0
setwd("~/Documents/Anjali Gupta/University of Auckland/Research/ESR/2.1Experiment-PostGrad/Bayesian Model")
#Model with Variable Variance dependent on fragment size - linear model, Size as the Explanatory Variable - V1
for(j in 2:ncol(data_size2)){
  element <- colnames(data_size2)[j]
  
  bugsData <- list(n = nrow(data_size2), x=data_size2[,1], y=data_size2[,j])
  
  bugsInits <- list(b0=0, b1=0)#, tau=1 ,  ,b2=0, b3=0
  bugsFile = "model_varfn2.0.R"
  parameters = c("b0", "b1","b2", "b3") #,, "p.b2.1", "p.b2.2", "p.b3.1", "p.b3.2","p.b3" "sigma2_i_med", "sigma2_i_mean", "sigma2_i_sd", "sigma", "sigma2",  "R2B", "sigma2_i"
  
  mod_samples <- paste(element, "samples", sep=".")
  mod_dic <- paste(element, "dic", sep=".")
  mod_hpd <- paste(element, "hpd", sep=".")
  mod_trace <- paste(element, "traceplot", "png", sep=".")
  mod_gelman <- paste(element, "gelman", "png", sep=".")
  mod_sigma2i <- paste(element, "sigma2i", sep=".")
  mod_sigma2i_hpd <- paste(element, "sigma2i", "hpd", sep=".")
  
  sim = jags.model(file=bugsFile, data=bugsData, inits=bugsInits, n.chains = 2)
  update(sim, 50000) #50000
  samples_mod = coda.samples(sim, parameters, 50000, thin = 10) #50000, thin = 50
  summary_mod <- summary(samples_mod)
  #Combining multiple chains in one for HPD Interval
  samples_mod_combined <- mcmc(do.call(rbind, samples_mod)) #Total 2000 values for each parameter
  #Get sigma2_i values together to get summary statistics and compute HPD interval of the same
  sigma2_i_val <- unname(summary_mod$statistics[4:40,"Mean"])
  sigma2_i_val_mcmc <- mcmc(sigma2_i_val)
  #varnames(samples_mod[[1]])
  #varnames(samples_mod[[2]])
  
  assign(mod_samples, print(summary(samples_mod)))
  assign(mod_sigma2i, print(summary(sigma2_i_val_mcmc)))
  assign(mod_dic, print(dic.samples(sim, 5000, "pD")))
  
  #sigma2_i[] = sigma2/x[]
  #mean(summary_mod$statistics[6:42,"Mean"]) #Match with sigma2_i_mean
  #sd(summary_mod$statistics[6:42,"Mean"]) #Match with sigma2_i_sd
  #median(summary_mod$statistics[6:42,"Mean"])
  assign(mod_hpd, print(HPDinterval(samples_mod_combined)))
  assign(mod_sigma2i_hpd, print(HPDinterval(sigma2_i_val_mcmc)))
  
  # Convergence Diagnostics
  png(mod_trace)
  par(mfrow=c(2,2))
  plot(samples_mod[,c('b2', 'b3')]) #,, 'p.b2.1', 'p.b3.1', ,'sigma2_i''b0','b1', , 'p.b2.1', 'p.b3.1'
  #plot(samples_mod[,'sigma2_i[2]'])
  dev.off()
  
  #par(mfrow=c(2, 1))
  traceplot(samples_mod[,'b2'], main = "Al 309: b2")
  traceplot(samples_mod[,'b3'], main = "Al 309: b3")
  plot(samples_mod[,'b2'], trace = FALSE, main = "Ba 493: b2")
  plot(samples_mod[,'b3'], trace = FALSE, main = "Ba 493: b3")
  
  png(mod_gelman)
  gelman.plot(samples_mod[,c('b0','b1','b2','b3')])#'b0','b1',
  dev.off()
  
  MCMCplot(samples_mod, params = "b3")
  #rmeanplot(samples_mod, parms=parameters)
  #autocorr.plot(samples_mod)
  #rejectionRate(samples_mod)
  #MCMCplot(samples_mod, params = "sr", ref_ovl = TRUE, rank = TRUE)
  #MCMCplot(samples_mod, params = "r", ref_ovl = TRUE, rank = TRUE)
}