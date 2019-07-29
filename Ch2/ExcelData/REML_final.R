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