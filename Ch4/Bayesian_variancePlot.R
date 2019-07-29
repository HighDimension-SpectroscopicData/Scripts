setwd("~/Documents/Anjali Gupta/University of Auckland/Research/ESR/2.1Experiment-PostGrad")

library('xlsx')

varResults <- read.xlsx('Bayesian_varianceResults.xlsx', header=TRUE, sheetIndex = 1)
elements_temp <- colnames(varResults)[3:ncol(varResults)]
elements <- unique(substr(elements_temp, start=1, stop=6))
elements[3] <- substr(elements[3], start=1, stop=5) #Need to do this for K 766

for(i in 1:length(elements)){
  element <- elements[i]
  
  mean_element <- paste(element, "mean", sep="_")
  ll_element <- paste(element, "ll", sep="_")
  ul_element <- paste(element, "ul", sep="_")
  q1_element <- paste(element, "2.5", sep="_")
  q3_element <- paste(element, "97.5", sep="_")
  max_y <- max(varResults[,q3_element])
  range_y <- c(0, max_y)
  
  plot_name <- paste(element, "var", "png", sep=".")
  
  png(plot_name)
  plot(varResults$Size_mm, varResults[,mean_element], type='l', ylim = range_y, 
       main=paste(element, "Variance Estimates vs Fragment Size", sep=": "), xlab="Fragment Size in mm", 
       ylab = "Variance Estimates")
  lines(varResults$Size_mm, varResults[,q1_element], col = 'red')
  lines(varResults$Size_mm, varResults[,q3_element], col = 'blue')
  legend("topright", col=c("red", "black", "blue"), lty= c(1,1,1), c("Quantile at 2.5%", "Mean", "Quantile at 97.5%"))
  dev.off()
}