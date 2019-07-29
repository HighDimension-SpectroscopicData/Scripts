library(lme4)
setwd("~/Documents/Anjali Gupta/University of Auckland/Research/ESR/1.Experiment_VariationComparison/ExcelData")

intensities <- read.csv('ExperimentsData.csv', header=TRUE, sep=',')
str(intensities)
intensities$Days <- as.factor(intensities$Days)
intensities$Runs <- as.factor(intensities$Runs)
intensities$Sample <- as.factor(intensities$Sample)

for(i in 4:ncol(intensities)){
  element <- colnames(intensities)[i]
  # RCBD
  #mod.aov <- aov(intensities[,i]~Error(intensities$Days/(intensities$Runs)))
  # REML
  model1 <- lmer(intensities[,i]~1+(1|intensities$Days)+(1|intensities$Days:intensities$Runs), REML=TRUE)
  
  #### Split Plot Design
  #### mod.aov <- aov(intensities[,i]~Error(intensities$Days/(intensities$Runs)/(intensities$Sample)))
  assign(paste("mod", element, sep="_"), print(model1))
  assign(paste("mod_summary", element, sep="_"), print(summary(model1)))
}

## Modification: REML to log of normalised data
intensities_v2_temp <- intensities[,4:14]/intensities[,15]
intensities_v2 <- data.frame(intensities[,1:3], intensities_v2_temp)
intensities_v2[,4:14] <- log(intensities_v2[,4:14])
for(i in 4:ncol(intensities_v2)){
  element <- colnames(intensities_v2)[i]
  # RCBD
  #mod.aov <- aov(intensities_v2[,i]~Error(intensities_v2$Days/(intensities_v2$Runs)))
  # REML
  model1 <- lmer(intensities_v2[,i]~1+(1|intensities_v2$Days)+(1|intensities_v2$Days:intensities_v2$Runs), REML=TRUE)
  
  # Split Plot Design
  # mod.aov <- aov(intensities[,i]~Error(intensities$Days/(intensities$Runs)/(intensities$Sample)))
  assign(paste("mod", element, sep="_"), print(model1))
  assign(paste("mod_summary", element, sep="_"), print(summary(model1)))
}

###### MODEL 2: Another model of the form
###### y = mean_k + theta_i + e_ij, where e_ij consists of Var_Runs, 
###### mean_k <- mean for each sample

## For original intensities
intensities <- read.csv('ExperimentsData_WithOrder.csv', header=TRUE, sep=',')
str(intensities)
intensities$Days <- as.factor(intensities$Days)
intensities$Runs <- as.factor(intensities$Runs)
intensities$Samples <- as.factor(intensities$Samples)
for(i in 4:ncol(intensities)){
  element <- colnames(intensities)[i]
  # RCBD
  #mod.aov <- aov(intensities[,i]~Error(intensities$Days/(intensities$Runs)))
  # REML
  ### Technically correct model: But lme package is poor.
  ### model1 <- lmer(intensities[,i]~intensities$Samples+(1|intensities$Days)+(1|intensities$Days:intensities$Runs), REML=TRUE)
  model1 <- lmer(intensities[,i]~intensities$Samples+(1|intensities$Days), REML=TRUE)
  
  #### Split Plot Design
  #### mod.aov <- aov(intensities[,i]~Error(intensities$Days/(intensities$Runs)/(intensities$Sample)))
  #assign(paste("mod", element, sep="_"), print(model1))
  assign(paste("mod_summary", element, sep="_"), print(summary(model1)))
}


## Modification: REML to log of normalised data
intensities <- read.csv('ExperimentsData_WithOrder.csv', header=TRUE, sep=',')
str(intensities)
intensities$Days <- as.factor(intensities$Days)
intensities$Runs <- as.factor(intensities$Runs)
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
  model1 <- lmer(intensities_v2[,i]~intensities_v2$Samples+(1|intensities_v2$Days), REML=TRUE)
  
  # Split Plot Design
  # mod.aov <- aov(intensities[,i]~Error(intensities$Days/(intensities$Runs)/(intensities$Sample)))
  #assign(paste("mod", element, sep="_"), print(model1))
  assign(paste("mod_summary", element, sep="_"), print(summary(model1)))
}
