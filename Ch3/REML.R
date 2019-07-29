setwd("~/Documents/Anjali Gupta/University of Auckland/Research/ESR/2.Experiment-PostGrad/Data")

#install.packages('lme4')
library(lme4)

## Modification: REML to log of normalised data
intensities <- read.csv('Peaks_Original_withOrder.csv', header=TRUE)
str(intensities)

# New command 1
intensities$typeofglass <- substr(intensities$samples, start=1, stop=(nchar(as.character(intensities$samples))-1))
intensities$glass_samples <- substr(intensities$samples, start=nchar(as.character(intensities$samples)), stop=nchar(as.character(intensities$samples)))
intensities$typeofglass <- as.factor(intensities$typeofglass)
intensities$glass_samples <- as.factor(intensities$glass_samples)

intensities$days <- as.factor(intensities$days)

## New command 2
intensities$replications <- as.factor(intensities$replications)

intensities$runs <- as.factor(intensities$runs)
intensities$order <- as.factor(intensities$order)

intensities_v2_temp <- intensities[,7:17]/intensities[,18]

# Edited the command below
intensities_v2 <- data.frame(intensities[,c(1:6, 19, 20)], intensities_v2_temp)
intensities_v2[,9:19] <- log(intensities_v2[,9:19])

## New command
library(lattice)
xyplot(intensities_v2$replications~intensities_v2$Al.394.Avg|intensities_v2$days*intensities_v2$typeofglass)
xyplot(intensities_v2$days~intensities_v2$Al.394.Avg|intensities_v2$samples)

dotplot(1:144~intensities_v2$Al.394.Avg|intensities_v2$samples, groups=intensities_v2$days, pch=c(1:4))

for(i in 9:ncol(intensities_v2)){
  
  element <- colnames(intensities_v2)[i]
  
  # New commands and Edited the command below
  model1 <- lmer(intensities_v2[,i]~intensities_v2$typeofglass+ (1|intensities_v2$typeofglass:intensities_v2$glass_samples)+
                   (1|intensities_v2$days)+ (1|intensities_v2$typeofglass:intensities_v2$days)+
                   (1|intensities_v2$typeofglass:intensities_v2$glass_samples:intensities_v2$days), REML = FALSE)
    
  #model2 <- lmer(intensities_v2[,i]~intensities_v2$typeofglass+ (1|intensities_v2$typeofglass:intensities_v2$glass_samples)+
  #(1|intensities_v2$days)+(1|intensities_v2$typeofglass:intensities_v2$glass_samples:intensities_v2$days), REML=FALSE)
  
  model3 <- lmer(intensities_v2[,i]~intensities_v2$typeofglass+ (1|intensities_v2$typeofglass:intensities_v2$glass_samples)+
                   (1|intensities_v2$days), REML=FALSE)
  
  #model3 is better than model1, model2
  #model4 <- lmer(intensities_v2[,i]~intensities_v2$typeofglass+ (1|intensities_v2$days), REML=FALSE)
  #model3 is better than model4
  
  #model5 <- lmer(intensities_v2[,i]~intensities_v2$typeofglass+ (1|intensities_v2$typeofglass:intensities_v2$glass_samples), REML=FALSE)
  #model3 is better than model5
  
  # Model1 REML is not needed as Model3 is better than Model1.
  # model1_reml <- lmer(intensities_v2[,i]~intensities_v2$typeofglass+ (1|intensities_v2$typeofglass:intensities_v2$glass_samples)+(1|intensities_v2$days)+ 
    #(1|intensities_v2$typeofglass:intensities_v2$days)+(1|intensities_v2$typeofglass:intensities_v2$glass_samples:intensities_v2$days), REML=TRUE)
  
  model3_reml <- lmer(intensities_v2[,i]~intensities_v2$typeofglass+ (1|intensities_v2$typeofglass:intensities_v2$glass_samples)+
                        (1|intensities_v2$days), REML=TRUE)
  
  assign(paste("anova",element, sep="_"), print(anova(model1, model3)))

  ## New command 4
  #install.packages('car')
  library(car)
  Anova(model3)
  #assign(paste("Anova",element, sep="_"), print(Anova(model3)))

  assign(paste("mod_summary", element, sep="_"), print(summary(model3_reml)))
}

write.csv(intensities_v2, "Peaks_Transformed_withOrder.csv", row.names = FALSE)

#Al309 - model3, type of glass - significant
#Al394 - model3, type of glass - not significant
#Ba493 - model3, type of glass - significant
#Ca534 - model3, type of glass - significant
#Ca643 - model3, type of glass - significant
#Fe373 - model3, type of glass - significant
#K766 - model3, type of glass - significant
#Na330 - model3, type of glass - significant
#Na818 - model3, type of glass - significant
#Sr407 - model3, type of glass - not significant
#Sr460 - model3, type of glass - not significant