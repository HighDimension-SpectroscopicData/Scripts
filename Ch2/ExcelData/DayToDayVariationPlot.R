setwd("~/Documents/Anjali Gupta/University of Auckland/Research/ESR/Experiment_VariationComparison/ExcelData")

days <- 3:5
var_days_Al309 <- c(203200549.3, 101333333.3, 97532622.92)
var_days_Al394 <- c(55143772.5, 24805183.17, 18563693.25)
var_days_Ba493 <- c(3058621, 2023153.833, 1679063.917)
var_days_Ca534 <- c(130564784.3, 68148808.92, 62083333.33)
data_var <- as.data.frame(days)
data_var <- cbind(data_var, var_days_Al309, var_days_Al394, var_days_Ba493, var_days_Ca534)

ylab1 <- parse(text=paste("Variation (10", "^7)"))
pdf(paste("DayToDayVariationPlot_v3", "pdf", sep="."))#, family = "Calibri Light"
plot(days, var_days_Al394/10000000, col="blue", cex=0.75, xlim=c(3,5), ylim=c(min(var_days_Al394/10000000, var_days_Al309/10000000), max(var_days_Al394/10000000, var_days_Al309/10000000)), las=1, 
     xlab="", ylab="", main="Day to Day Variation", axes=F, pch = 19)
points(days, var_days_Al309/10000000, col="red", cex=0.75, pch = 19)
#points(days, var_days_Ba493/10000000, col="green", cex=0.5, pch = 19)
points(days, var_days_Ca534/10000000, col="green", cex=0.75, pch = 19)
legend("topright", legend=c("Al394", "Al309", "Ca534"), col=c("blue", "red", "green"), bty="o", cex=1, pch=19, pt.cex=0.75)#"green" "Ba493",
box()
axis(1, at=c(3,4,5), labels=seq(3,5,1))
axis(2, at=seq(0,20,5), las=1)
title(ylab=ylab1, line=2.5, cex.lab=1)
title(xlab="Days", line=2.5, cex.lab=1)
graphics.off()
