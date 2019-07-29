#install.packages('rjags')
library(rjags)

setwd("~/Documents/Anjali Gupta/University of Auckland/Research/ESR/Experiment_VariationComparison/Bayesian_VarComp")
intensities <- read.csv('ExperimentsData.csv', header=TRUE, sep=',')
str(intensities)

intensities$Days <- as.factor(intensities$Days)
intensities$Runs <- as.factor(intensities$Runs)
intensities$Sample <- as.factor(intensities$Sample)
intensity <- intensities[,13] # 8-Ca534, 6-K766, 9-Sr460 # Normal ones 12-Al309, 13-Ba493
#14-Sr407

bugsData = list(intensity = intensity, blocks = intensities$Days, runs = intensities$Runs, nr = 3, nb = 5, ns = 4)#N = length(intensity),
bugsInits = list(list(b = rep(0, 5), r = matrix(0, nrow=5, ncol=3), sigma.b = 1300, sigma.r=360, sigma = 2200, mu.bar=0))
# Inits for Uniform Prior for Al309 - sigma.b = 9000, sigma.r=2000, sigma = 20000
# Inits for Inv-Gamma Prior for Al309 - tau.b = 1e-08, tau.r=2.4e-07, tau = 1.3e-09 
bugsFile = "rcbd_aov_bugs.R"

parameters = c("sigma.b.sq", "sigma.r.sq", "sigma.sq", "mu.bar")
sim = jags.model(file=bugsFile, data=bugsData, inits=bugsInits)
update(sim, 1500000)
samples = coda.samples(sim, parameters, 1000000, thin = 100)
summary(samples)

# Convergence Diagnostics
jpeg('Trace_DensityPlots.jpeg')
plot(samples)
dev.off()

#install.packages("mcmcplots")
library(mcmcplots)
rmeanplot(samples, parms=parameters)

autocorr.plot(samples)

rejectionRate(samples)

length(samples[[1]]) 
summary(samples[[1]][1:1000]) #mu.bar
#Min.   1st Qu.  Median    Mean 3rd Qu.    Max. 
#184400  204400  207300  207200  210400  223200 
summary(samples[[1]][1001:2000]) #sigma.b.sq
#Min.       1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.000e+00 1.200e+01 4.831e+04 5.006e+07 2.049e+07 2.660e+09
summary(samples[[1]][2001:3000]) #sigma.r.sq
#Min.   1st Qu.    Median      Mean     3rd Qu.      Max. 
#0         1       718      13850000    404400 926700000 
summary(samples[[1]][3001:4000]) #sigma.sq
#Min.       1st Qu.    Median      Mean   3rd Qu.      Max. 
#4.774e+08 7.206e+08 8.133e+08 8.326e+08 9.238e+08 1.514e+09 