setwd("~/Documents/Anjali Gupta/University of Auckland/Research/ESR/Experiment_VariationComparison/ExcelData")
intensities <- read.csv('ExperimentsData.csv', header=TRUE, sep=',')
str(intensities)
intensities$Days <- as.factor(intensities$Days)
intensities$Runs <- as.factor(intensities$Runs)
intensities$Sample <- as.factor(intensities$Sample)
for(i in 4:ncol(intensities)){
  element <- colnames(intensities)[i]
  # RCBD
  mod.aov <- aov(intensities[,i]~Error(intensities$Days/(intensities$Runs)))
  # Split Plot Design
  # mod.aov <- aov(intensities[,i]~Error(intensities$Days/(intensities$Runs)/(intensities$Sample)))
  assign(paste("mod", element, sep="_"), print(mod.aov))
  assign(paste("mod_summary", element, sep="_"), print(summary(mod.aov)))
}

## Variance of Days 1,2, 1,2,3, 1,2,3,4, 1,2,3,4,5
for(j in 2:5){
 for(i in 4:ncol(intensities)){  
  element <- colnames(intensities)[i]
  mod.aov <- aov(intensities[1:(j*12),i]~Error(intensities$Days[1:(j*12)]/(intensities$Runs[1:(j*12)])))
  assign(paste("mod", j, element, sep="_"), print(mod.aov))
  assign(paste("mod_summary", j, element, sep="_"), print(summary(mod.aov)))
 }
}

## Test - Another Modification : No Normalisation, but take log
intensities[,4:15] <- log(intensities[,4:15])
for(i in 4:ncol(intensities)){
  element <- colnames(intensities)[i]
  # RCBD
  mod.aov <- aov(intensities[,i]~Error(intensities$Days/(intensities$Runs)))
  # Split Plot Design
  # mod.aov <- aov(intensities[,i]~Error(intensities$Days/(intensities$Runs)/(intensities$Sample)))
  assign(paste("mod", element, sep="_"), print(mod.aov))
  assign(paste("mod_summary", element, sep="_"), print(summary(mod.aov)))
}

## Test - Another Modification : If Dividing throughout by Silicon Makes it Better
## Divide through by the Silicon intensity and then take the log
intensities_v2_temp <- intensities[,4:14]/intensities[,15]
intensities_v2 <- data.frame(intensities[,1:3], intensities_v2_temp)
intensities_v2[,4:14] <- log(intensities_v2[,4:14])
for(i in 4:ncol(intensities_v2)){
  element <- colnames(intensities_v2)[i]
  # RCBD
  mod.aov <- aov(intensities_v2[,i]~Error(intensities_v2$Days/(intensities_v2$Runs)))
  # Split Plot Design
  # mod.aov <- aov(intensities[,i]~Error(intensities$Days/(intensities$Runs)/(intensities$Sample)))
  assign(paste("mod", element, sep="_"), print(mod.aov))
  assign(paste("mod_summary", element, sep="_"), print(summary(mod.aov)))
}
write.csv(intensities_v2, "ExperimentsData_Normalised.csv", row.names = FALSE)

## Test for Sample Effect (Fixed)
for(i in 4:ncol(intensities)){
  element <- colnames(intensities)[i]
  # RCBD
  #mod.aov <- aov(intensities[,i]~intensities$Sample+Error(intensities$Days/(intensities$Runs)))
  mod.aov <- aov(intensities[,i]~intensities$Sample+Error(intensities$Days/intensities$Runs))
  # Split Plot Design
  # mod.aov <- aov(intensities[,i]~Error(intensities$Days/(intensities$Runs)/(intensities$Sample)))
  assign(paste("mod", element, sep="_"), print(mod.aov))
  assign(paste("mod_summary", element, sep="_"), print(summary(mod.aov)))
}
## Result: For some elements, sample effect is significant, and for the others,
## its not.

## Test - log of transformed data - ML fit of the model with sample effect
## (Test - Another Modification : If Dividing throughout by Silicon Makes it Better
## Divide through by the Silicon intensity and then take the log)
intensities_v2_temp <- intensities[,4:14]/intensities[,15]
intensities_v2 <- data.frame(intensities[,1:3], intensities_v2_temp)
intensities_v2[,4:14] <- log(intensities_v2[,4:14])
for(i in 4:ncol(intensities_v2)){
  element <- colnames(intensities_v2)[i]
  # RCBD
  mod.aov <- aov(intensities_v2[,i]~intensities_v2$Sample+Error(intensities_v2$Days/(intensities_v2$Runs)))
  # Split Plot Design
  # mod.aov <- aov(intensities[,i]~Error(intensities$Days/(intensities$Runs)/(intensities$Sample)))
  #assign(paste("mod", element, sep="_"), print(mod.aov))
  assign(paste("mod_summary", element, sep="_"), print(summary(mod.aov)))
}

## Result: For some elements, sample effect is significant, and for the others,
## its not.

## Another Modification: Subtract sample means for each element, and then
## fit the model to residuals.
samplemeans <- aggregate(intensities[,i:ncol(intensities)], by=list(intensities$Sample), mean)
intensities_v2 <- intensities[,1:3]
for(i in 4:ncol(intensities)){
  element <- colnames(intensities)[i]
  intensities_v2$resid[intensities_v2$Sample==1] <- intensities[intensities$Sample==1,i]-samplemeans[1,i-2]
  intensities_v2$resid[intensities_v2$Sample==2] <- intensities[intensities$Sample==2,i]-samplemeans[2,i-2]
  intensities_v2$resid[intensities_v2$Sample==3] <- intensities[intensities$Sample==3,i]-samplemeans[3,i-2]
  intensities_v2$resid[intensities_v2$Sample==4] <- intensities[intensities$Sample==4,i]-samplemeans[4,i-2]
  colnames(intensities_v2)[i] <- element
  
  # RCBD
  #mod.aov <- aov(intensities_v2[,i]~intensities_v2$Sample+Error(intensities_v2$Days/(intensities_v2$Runs)))
  ## Eliminated Sample Effect
  mod.aov <- aov(intensities_v2[,i]~Error(intensities_v2$Days/(intensities_v2$Runs)))
  assign(paste("mod_summary", element, sep="_"), print(summary(mod.aov)))
}

write.csv(intensities_v2, "ExperimentsData_SampleEffectRemoved.csv", row.names = FALSE)

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
  mod.aov <- aov(intensities[,i]~intensities$Samples+Error(intensities$Days/intensities$Runs))
  # Split Plot Design
  # mod.aov <- aov(intensities[,i]~Error(intensities$Days/(intensities$Runs)/(intensities$Sample)))
  # assign(paste("mod", element, sep="_"), print(mod.aov))
  assign(paste("mod_summary", element, sep="_"), print(summary(mod.aov)))
}

## Test - log of transformed data - ML fit of the model with sample effect
intensities_v2_temp <- intensities[,4:14]/intensities[,15]
intensities_v2 <- data.frame(intensities[,1:3], intensities_v2_temp)
intensities_v2[,4:14] <- log(intensities_v2[,4:14])
for(i in 4:ncol(intensities_v2)){
  element <- colnames(intensities_v2)[i]
  # RCBD
  mod.aov <- aov(intensities_v2[,i]~intensities_v2$Samples+Error(intensities_v2$Days/(intensities_v2$Runs)))
  # Split Plot Design
  # mod.aov <- aov(intensities[,i]~Error(intensities$Days/(intensities$Runs)/(intensities$Sample)))
  #assign(paste("mod", element, sep="_"), print(mod.aov))
  assign(paste("mod_summary", element, sep="_"), print(summary(mod.aov)))
}
