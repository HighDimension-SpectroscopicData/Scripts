install.packages('rjags')
library(rjags)

setwd("~/Documents/Anjali Gupta/University of Auckland/Research/ESR/2.Experiment-PostGrad/Data")

intensities <- read.csv('Peaks_Transformed_withOrder.csv', header=TRUE, sep=',')

intensities$days <- as.factor(intensities$days)
intensities$replications <- as.factor(intensities$replications)
intensities$runs <- as.factor(intensities$runs)
intensities$order <- as.factor(intensities$order)
intensities$glass_samples <- as.factor(intensities$glass_samples)


intensity <- intensities[,19] # 17 - not converged - Al309
element <- colnames(intensities)[19]

bugsData = list(intensity = intensity, days = intensities$days, samples = intensities$samples, type = intensities$typeofglass, ndays = length(unique(intensities$days)), nsamples = length(unique(intensities$samples)), ntype = length(unique(intensities$typeofglass)), N = length(intensity))
bugsInits = list(list(d = rep(0, 4), s = rep(0,12), mu.bar=rep(0, 4)))#sigma.b = 62200000, sigma = 487000000, tau.b = 1.6e-08, tau = 2e-09,

bugsFile = "~/Documents/Anjali Gupta/University of Auckland/Research/ESR/2.0Experiment-PostGrad/crosseddesign_bayesian_mod.R"

parameters = c( "sigma.d.sq", "sigma.sq", "sigma.s.sq", "mu.bar", "p.ratio", "ratio")
sim = jags.model(file=bugsFile, data=bugsData, inits=bugsInits)
update(sim, 50000)
samples_mod = coda.samples(sim, parameters, 100000, thin = 100)# 
summary(samples_mod)

# Convergence Diagnostics
#jpeg('Trace_DensityPlots.jpeg')
#plot(samples_mod)
#dev.off()

plot(density(samples_mod[[1]][,"sigma.d.sq"]), xlim=c(0,0.1))

jpeg(paste('Traceplot_', element, '.jpeg', sep=""))
par(mfrow=c(3,3))
traceplot(samples_mod)
dev.off()

jpeg(paste('Densityplot_', element, '.jpeg', sep=""))
par(mfrow=c(3,3))
densplot(samples_mod)
dev.off()

#install.packages("mcmcplots")
library(mcmcplots)
rmeanplot(samples_mod, parms=parameters)

autocorr.plot(samples_mod)

rejectionRate(samples_mod)

