#install.packages('lmtest')
library(lmtest) #for lrtest

setwd("~/Documents/Anjali Gupta/University of Auckland/Research/ESR/2.1Experiment-PostGrad")

data_size <- read.csv('Peaks_Original_Combined.csv', header=TRUE) #Peaks_Original.csv

data_size <- data_size[do.call(order, data_size),]

#Exploratory
for(i in 2:13){
  png(paste(colnames(data_size)[i], "png", sep="."))
  plot(data_size[,c(1, i)], xlab = "Size (mm)", ylab = "Intensities", main = colnames(data_size)[i], pch = 16)
  identify(data_size[,c(1, i)], pos = 4, plot = TRUE, labels = data_size[,1])
  #abline(lm(data_size[,i] ~ data_size[,1]))
  dev.off()
}


library(ggplot2)
ggplot(data_size) +
  geom_dotplot(aes(x=Size_mm), binwidth = .5, method="histodot") +
  theme_minimal()


#Sr407 - For all the fragments, this is negative except one. 
#Thus, all the negative entries have been replaced by the one positive entry.
#<1mm - 4 observations
#1-5mm - 20 observations, 5-20mm - 19 observations
#Al309, Na330, Sr460, Ca534, K766 - some pattern depicting that there maybe more 
#variation when the fragment size is small.

data_size2 <- data_size[,-12] #Removing Sr407

#write.csv(data_size2, 'Data_Transformed_temp.csv', row.names = FALSE) #Identify outliers
#Remove the data for 0.45mm fragment.

data_size2 <- data_size2[!(data_size2$Size_mm == 0.45),]

#Data Transformation
#Ratio wrt Si288, and log afterwards
data_size2[,2:11] <- data_size2[,2:11]/data_size2[,12]
data_size2 <- data_size2[,-12]
data_size2[,2:11] <- log(data_size2[,2:11])


#write.csv(data_size2, 'Data_Transformed.csv', row.names = FALSE)
#***********************************************************************************************

#Model
data_size2 <- read.csv('Data_Transformed.csv', header=TRUE)

setwd("~/Documents/Anjali Gupta/University of Auckland/Research/ESR/2.1Experiment-PostGrad/Model")
#Linear Model
for(i in 2:ncol(data_size2)){
  #Least Square
  mod_nopred <- lm(data_size2[,i] ~ 1)
  mod_constvar <- lm(data_size2[,i] ~ data_size2[,1])
  #Weighted Least Square
  mod <- lm(data_size2[,i] ~ data_size2[,1], weights=data_size2[,1])
  
  #plot(mod) #Visually, it seems like the data is heteroscadistic
  #Both the below tests do not work in our situation as the variance of the random errors are assumed to be inversely proportional to the size of the fragment.
  #bptest(mod_temp) #This test measures any linear heteroscadisticity in the data. p-value > 0.05. Thus, failure to reject null (variance is homoscedastic)
  #ncvTest(mod, ~ 1/data_size2[,1]) #p-value > 0.05, implying that the null cannot be rejected.
  
  #lrtest(mod, mod_constvar, mod_nopred)
  
  assign(paste(colnames(data_size2)[i], "mod", sep="_"), print(summary(mod)))
  assign(paste(colnames(data_size2)[i], "mod.ConstVar", sep="_"), print(summary(mod_constvar)))
  assign(paste(colnames(data_size2)[i], "mod.NoPred", sep="_"), print(summary(mod_nopred)))
  #Compute variable variance estimate
  assign(paste(colnames(data_size2)[i], "sigma2_i", sep="_"), mean((sigma(mod)^2)/data_size2[,1]))
  
  #Model Diagnostics
  
  #1. Plot Residuals against Predictors
  #Since, predictor is anyway not significant other than intercept
  #Plot just the residuals - the residuals should be scattered without showing any pattern.
  #Especially, around 0 on the y-axis. Also, the variance should be constant throughout.
  png(paste(colnames(data_size2)[i], "res", "png", sep="."))
  #plot(rstudent(mod_temp)) Studentized residuals - recommended, but doesn't make a different in this case.
  plot(mod$res)
  dev.off()
  
  #Plot of fitted values with residuals
  #plot(mod$fitted, mod$res)
    
  #Check normality of the residuals
  png(paste(colnames(data_size2)[i], "qq", "png", sep="."))
  qqnorm(mod$res)
  qqline(mod$res)
  dev.off()
  png(paste(colnames(data_size2)[i], "hist_res", "png", sep="."))
  hist(mod$res)
  dev.off()
  
  #Opening upward - Right skewed
  #Opening downward - Left skewed
  #S shape - Heavy tails
  #Inverted S shape -
  #Light Tails as compared to what is expected when data is simulated using Normal Distribution
}

#Cooks Distance - probably not needed for this experiment
#This is to recognise if there are any influential points that might be affecting the model fit significantly
#cook <- cooks.distance(mod)
#plot(cook)

data_size3 <- data_size2[-1,]
