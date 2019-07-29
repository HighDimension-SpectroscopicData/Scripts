#install.packages('rjags')
library(rjags)

setwd("~/Documents/Anjali Gupta/University of Auckland/Research/ESR/Experiment_VariationComparison/Bayesian_VarComp")
intensities <- read.csv('ExperimentsData_WithOrder.csv', header=TRUE, sep=',')
str(intensities)

intensities$Days <- as.factor(intensities$Days)
intensities$Runs <- as.factor(intensities$Runs)
intensities$Samples <- as.factor(intensities$Samples)

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
