setwd("~/Documents/Anjali Gupta/University of Auckland/Research/ESR/1.Experiment_VariationComparison/ExcelData")

intensities <- read.csv('ExperimentsData_SampleEffectRemoved.csv', header=TRUE, sep=',')
time <- read.csv('RandomNoForExp.csv', header=TRUE, sep=',')
intensities_v2 <- data.frame(Days=time$days, Runs=intensities$Runs, Samples=time$order)
intensities_v2$key <- apply(intensities_v2[,1:3], 1, paste, collapse="")
intensities$key <- apply(intensities[,1:3], 1, paste, collapse="")
intensities_orig <- within(intensities, rm(Days, Runs, Sample))
intensities_v2 <- merge(intensities_v2, intensities_orig, by="key", sort=FALSE)
intensities_v3 <- data.frame(matrix(NA, nrow = nrow(intensities_v2), ncol = ncol(intensities_v2)))
colnames(intensities_v3) <- colnames(intensities_v2)
intensities_v3[,1:4] <- intensities_v2[,1:4]

hanningsm <- function(y){
  y_sm <- rep(NA, length(y))
  for(j in 2:(length(y)-1)){
    y_sm[j] <- 0.25*y[j-1] + 0.5*y[j] + 0.25*y[j+1]
  }
  for(k in 1:5){
    y_sm[(k-1)*12+1] <- y[(k-1)*12+1]
    y_sm[k*12] <- y[k*12]
  }
  return(y_sm)
}

setwd("~/Documents/Anjali Gupta/University of Auckland/Research/ESR/1.Experiment_VariationComparison/ExcelData/Plots")
pdf("SampleEffectRemoved.pdf")
for(i in 5:16){
  a <- min(intensities_v2[,i])/1000
  b <- max(intensities_v2[,i])/1000
  ylab1 <- parse(text=paste("Residuals (10", "^3)"))
  
  intensities_v3[,i] <- hanningsm(intensities_v2[,i])
  colnames(intensities_v3)[i] <- colnames(intensities_v2)[i]
  
  #jpeg(paste(colnames(intensities_v2)[i], "SampleEffectRemoved", "jpeg", sep="."))
  pdf(paste(colnames(intensities_v2)[i], "SampleEffectRemoved", "pdf", sep="."))#, family = "Calibri Light"
  plot(1:60, intensities_v2[,i]/1000, xlab="", ylab="", main=colnames(intensities_v2)[i], col=c("orange", "red", "green", "blue")[intensities_v2$Samples], 
       cex=0.75, axes=FALSE, pch = c(19,19,19,19))
  legend("topright", col=c("orange", "red", "green", "blue"), legend=c("S1", "S2", "S3", "S4"), pch=c(19,19,19,19), cex=0.75)
  box()
  axis(1, at = c(6, 18, 30, 42, 54), labels = 1:5)
  axis(2, xlim=range(a,b), las=1)
  abline(v=c(12,24,36,48), lty="dashed")
  lines(1:12, intensities_v3[1:12,i]/1000, col = "red", lwd = 2)
  lines(13:24, intensities_v3[13:24,i]/1000, col = "red", lwd = 2)
  lines(25:36, intensities_v3[25:36,i]/1000, col = "red", lwd = 2)
  lines(37:48, intensities_v3[37:48,i]/1000, col = "red", lwd = 2)
  lines(49:60, intensities_v3[49:60,i]/1000, col = "red", lwd = 2)
  title(ylab=ylab1, line=2.5, cex.lab=1)
  title(xlab="Days", line=2.5, cex.lab=1)
  graphics.off()
  #dev.off()
}
graphics.off()

# For Na.330

plot(intensities_v2$Na.330.Avg, type='l', main="Na.330", ylab="Residuals")
lines(intensities_v3$Na.330.Avg, type='l', col="red", lty=2)
abline(v=c(12,24,36,48), lty="dashed")
legend("topright", legend=c("Residuals", "Smooth Residuals"), lty=c(1,2), col=c("black", "red"), cex=0.75)
