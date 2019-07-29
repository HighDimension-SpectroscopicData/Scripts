setwd("~/Documents/Anjali Gupta/University of Auckland/Research/ESR/2.Experiment-PostGrad/Data")

#install.packages(lme4)
library(lme4)

## Modification: REML to log of normalised data
intensities <- read.csv('ExperimentsData_WithOrder.csv', header=TRUE, sep=',')
str(intensities)
intensities$Days <- as.factor(intensities$Days)
intensities$Runs <- as.factor(intensities$Runs)
## Convert TypeOfGlass as factor ----
intensities$Samples <- as.factor(intensities$Samples)

intensities_v2_temp <- intensities[,4:14]/intensities[,15]
intensities_v2 <- data.frame(intensities[,1:3], intensities_v2_temp)
intensities_v2[,4:14] <- log(intensities_v2[,4:14])
for(i in 4:ncol(intensities_v2)){
  element <- colnames(intensities_v2)[i]
  # RCBD
  #mod.aov <- aov(intensities_v2[,i]~Error(intensities_v2$Days/(intensities_v2$Runs)))
  # REML
  ### Technically correct model: But lme package is poor.
  ### model1 <- lmer(intensities_v2[,i]~intensities_v2$Samples+(1|intensities_v2$Days)+(1|intensities_v2$Days:intensities_v2$Runs), REML=TRUE)
  model1 <- lmer(intensities_v2[,i]~intensities_v2$TypeOfGlass+intensities_v2$TypeOfGlass:intensities_v2$Samples+(1|intensities_v2$Days), REML=TRUE)
  
  # Split Plot Design
  # mod.aov <- aov(intensities[,i]~Error(intensities$Days/(intensities$Runs)/(intensities$Sample)))
  #assign(paste("mod", element, sep="_"), print(model1))
  assign(paste("mod_summary", element, sep="_"), print(summary(model1)))
}
