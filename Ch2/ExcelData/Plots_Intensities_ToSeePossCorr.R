setwd("~/Documents/Anjali Gupta/University of Auckland/Research/ESR/Experiment_VariationComparison/ExcelData")

intensities <- read.csv('ExperimentsData.csv', header=TRUE, sep=',')
time <- read.csv('RandomNoForExp.csv', header=TRUE, sep=',')
intensities_v2 <- data.frame(Days=time$days, Runs=intensities$Runs, Samples=time$order)
intensities_v2$key <- apply(intensities_v2[,1:3], 1, paste, collapse="")
intensities$key <- apply(intensities[,1:3], 1, paste, collapse="")
intensities_orig <- within(intensities, rm(Days, Runs, Sample))
intensities_v2 <- merge(intensities_v2, intensities_orig, by="key", sort=FALSE)

write.csv(intensities_v2[,-1], "ExperimentsData_WithOrder.csv", row.names=FALSE)

#install.packages('extrafont')
#library(extrafont)
#font_import()
loadfonts()

for(i in 5:16){
a <- min(intensities_v2[,i])/10000
b <- max(intensities_v2[,i])/10000
ylab1 <- parse(text=paste("Intensity (10", "^4)"))

#jpeg(paste(colnames(intensities_v2)[i], "jpeg", sep="."))
pdf(paste(colnames(intensities_v2)[i], "pdf", sep="."))#, family = "Calibri Light"
plot(1:60, intensities_v2[,i]/10000, xlab="", ylab="", main=colnames(intensities_v2)[i],
     col=c("orange", "red", "green", "blue")[intensities_v2$Samples], cex=0.75, axes=FALSE, pch = c(19,19,19,19))
title(ylab=ylab1, mgp=c(2.5,1,0),cex.lab=1)#, family="Calibri Light"
title(xlab="Days", mgp=c(2.5,1,0),cex.lab=1)#, family="Calibri Light"
legend("topright", col=c("orange", "red", "green", "blue"), legend=c("S1", "S2", "S3", "S4"), pch=c(19,19,19,19), cex=0.75)
box()
axis(1, at = c(6, 18, 30, 42, 54), labels = 1:5)
axis(2, xlim=range(a,b), las=1)
abline(v=c(12,24,36,48), lty="dashed")
graphics.off()
#dev.off()
}