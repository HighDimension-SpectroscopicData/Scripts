#install.packages('rjags')
library(rjags)

setwd("~/Documents/Anjali Gupta/University of Auckland/Research/ESR/Experiment_VariationComparison/Bayesian_VarComp")
intensities <- read.csv('ExperimentsData_WithOrder.csv', header=TRUE, sep=',')
str(intensities)

intensities$Days <- as.factor(intensities$Days)
intensities$Runs <- as.factor(intensities$Runs)
intensities$Samples <- as.factor(intensities$Samples)

#### Model 2 - Original Data

intensity <- intensities[,12] # 8-Ca534, 6-K766, 9-Sr460 # Normal ones 12-Al309, 13-Ba493
#14-Sr407

bugsData = list(intensity = intensity, blocks = intensities$Days, runs = intensities$Runs, samples = intensities$Samples, nb = 5, nr = 12, ns = 4, N = length(intensity))
bugsInits = list(list(b = rep(0, 5), mu.bar=rep(0, 4)))#sigma.b = 62200000, sigma = 487000000, tau.b = 1.6e-08, tau = 2e-09,
# Inits for Uniform Prior for Al309 - sigma.b = 9000, sigma.r=2000, sigma = 20000
# Inits for Inv-Gamma Prior for Al309 - tau.b = 1e-08, tau.r=2.4e-07, tau = 1.3e-09 
bugsFile = "rcbd_aov_bugs_transformeddata.R"

parameters = c("sigma.b.sq", "sigma.sq", "mu.bar")
sim = jags.model(file=bugsFile, data=bugsData, inits=bugsInits)
update(sim, 500000)
samples_mod = coda.samples(sim, parameters, 500000, thin = 100)
summary(samples_mod)

#### Model 2 - Transformed Data
intensities_v2_temp <- intensities[,4:14]/intensities[,15]
intensities_v2 <- data.frame(intensities[,1:3], intensities_v2_temp)
intensities_v2[,4:14] <- log(intensities_v2[,4:14])

intensity <- intensities_v2[,14]

bugsData = list(intensity = intensity, blocks = intensities_v2$Days, runs = intensities_v2$Runs, samples = intensities_v2$Samples, nb = 5, nr = 12, ns = 4, N = length(intensity))
bugsInits = list(list(b = rep(0, 5), mu.bar=rep(0, 4)))#sigma.b = 62200000, sigma = 487000000, tau.b = 1.6e-08, tau = 2e-09,

bugsFile = "rcbd_aov_bugs_transformeddata2.R"

parameters = c("sigma.b.sq", "sigma.sq", "mu.bar", "ratio", "p.ratio")
sim = jags.model(file=bugsFile, data=bugsData, inits=bugsInits)
update(sim, 200000)
samples_mod = coda.samples(sim, parameters, 100000, thin = 100)
summary(samples_mod)


# Convergence Diagnostics
jpeg('Trace_DensityPlots.jpeg')
plot(samples_mod)
dev.off()

#install.packages("mcmcplots")
library(mcmcplots)
rmeanplot(samples_mod, parms=parameters)

autocorr.plot(samples_mod)

rejectionRate(samples_mod)
