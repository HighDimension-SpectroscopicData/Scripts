setwd("~/Documents/Anjali Gupta/University of Auckland/Research/ESR/1.Experiment_VariationComparison/ExcelData")

data <- read.csv('TransIntensities_Model2_AOV_Variation_v3.csv', header=TRUE)
colnames(data)[1] <- "Elements"

bp <- barplot(data[2:11,2], main="Ratio of Inter-Day and Intra-Day Variation", horiz=TRUE, 
              xlim=c(0,1.5), col='pink', xlab="", ylab="", las=1)
box()
#names.arg=data[,1], cex.names=0.5
abline(v=1, lty=2)
grid()

text(data[2:11,2]+0.12, seq(0.7, 11.5, 1.2), data[2:11,1], cex=0.75)
title(ylab="Elements", line=0.7)
title(xlab="Ratio", line=2.2)
