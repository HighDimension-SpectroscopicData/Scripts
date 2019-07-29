library(MASS)
library(dichromat)
source("Tippett_function.R")

LR.H1.exp <- read.table("./Parallel Processing/Mean Log LR/100RPs/Iteration 1/3_comparison_research_Nor_same_logLR.txt")
LR.H2.exp <- read.table("./Parallel Processing/Mean Log LR/100RPs/Iteration 1/3_comparison_research_Nor_different_logLR.txt")

#Histograms
#par(mfrow=c(1,2))
#histdiff <- truehist(diff[,1], main="Log LR Distribution: Different Source Comparison", xlab="log(Geometric Mean of 100 LRs)", ylab="Density", col = 8)
#histsame <- truehist(same[,1], main="Log LR Distribution: Within-Same Source Comparison", xlab="log(Geometric Mean of 100 LRs)", ylab="Density", col = 5)
#dev.off()

par(mfrow=c(1,2))
pdf("logLR_diffsources7.pdf", width = 8.27, height = 5.83) 
histdiff <- hist(LR.H2.exp[,1], breaks = 34, plot=FALSE)
#my_col_diff <- ifelse(histdiff$breaks >= 0, "red", "gray")
my_col_diff <- ifelse(histdiff$breaks >= 0, "red", "#CCBFFF")# #FFFF99
#my_col_diff <- ifelse(histdiff$breaks >= 0, dichromat("#FFFF99"), dichromat("#CCBFFF"))
plot(histdiff, col=my_col_diff, freq = FALSE, main="Log LR Distribution: Different Source Comparison", 
     xlab="", ylab="")
title(ylab="Density", mgp=c(2.5,1,0),cex.lab=1)
title(xlab="log(Geometric Mean of 100 LRs)", mgp=c(2.5,1,0),cex.lab=1)
abline(v = 0, lty = 2, lwd = 2)
graphics.off()

pdf("logLR_samesource7.pdf", width = 8.27, height = 5.83) 
histsame <- hist(LR.H1.exp[,1], breaks = 26, plot = FALSE)
#my_col_same <- ifelse(histsame$breaks < 0, "red", "light blue")
my_col_same <- ifelse(histsame$breaks < 0, "red", "#664CFF") # #FFFF99
#my_col_same <- ifelse(histsame$breaks < 0, dichromat("#FFFF99"), dichromat("#664CFF"))
plot(histsame, col=my_col_same, freq = FALSE, main="Log LR Distribution: Within-Same Source Comparison", 
     xlab="", ylab="")
title(ylab="Density", mgp=c(2.5,1,0),cex.lab=1)
title(xlab="log(Geometric Mean of 100 LRs)", mgp=c(2.5,1,0),cex.lab=1)
abline(v=0, lty=2, lwd = 2)
#dev.off()
graphics.off()

#Tippett plot
pdf("TippettPlot_1.pdf", width = 8.27, height = 5.83)
Tippett_plot(LR.H1.exp, LR.H2.exp) ##plotting the Tippett plot
graphics.off()

#Histogram - both same and different log likelihoods on the same histogram
lr_comb <- c(LR.H1.exp[,1], LR.H2.exp[,1])
hist(LR.H1.exp[,1], col = "#664CFF", freq = FALSE, breaks = pretty(lr_comb, n = 60), 
     main = "Log LR Distribution", xlab = "log(Geometric Mean of 100 LRs)", ylim = c(0, 0.10), xlim = c(-50, 15))#, ylim = c(0, 0.10), xlim = c(-50, 15)
hist(LR.H2.exp[,1], col = rgb(204/255,191/255,255/255,0.5), add=TRUE, freq = FALSE, breaks = pretty(lr_comb, n = 80))##CCBFFF
abline(v = 0, lty = 2, lwd = 2)
text(x = -1, y = 0.04, "LR = 1", srt = 90)#x = -2, y = 0.2
