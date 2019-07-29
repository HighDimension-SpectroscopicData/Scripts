#setwd("~/Documents/Anjali Gupta/University of Auckland/Research/ESR/4.Experiment-AitkenLucy/BuildingGlass/Using scripts in Zadora's book")

data_peaks <- read.csv("BG_Peaks_Original.csv", header = TRUE)

## Replacing negative observations with the minimum observation recorded for the peak
summary(data_peaks[(which(data_peaks[ ,11] < 0)), 11])
summary(data_peaks[(which(data_peaks[ ,11] >= 0)), 11])
summary(data_peaks[(which(data_peaks[ ,12] < 0)), 12])
summary(data_peaks[(which(data_peaks[ ,12] >= 0)), 12])
summary(data_peaks[(which(data_peaks[ ,13] < 0)), 13])
summary(data_peaks[(which(data_peaks[ ,13] >= 0)), 13])
summary(data_peaks[(which(data_peaks[ ,14] < 0)), 14])
summary(data_peaks[(which(data_peaks[ ,14] >= 0)), 14])

peaks_w_neg <- apply(data_peaks[,4:15], 2, min)
cols_w_neg <- which(peaks_w_neg < 0) + 3

data_peaks_pos <- data_peaks

for(i in 1:length(cols_w_neg)){
  cols_replace <- min(data_peaks[which(data_peaks[,cols_w_neg[i]] >= 0), cols_w_neg[i]])
  data_peaks_pos[,cols_w_neg[i]] <- ifelse(data_peaks_pos[,cols_w_neg[i]]<0, cols_replace, data_peaks_pos[,cols_w_neg[i]])
}

elements <- colnames(data_peaks_pos)[4:ncol(data_peaks_pos)]

for(i in 1:length(elements)){
  #png(paste(elements[i], "hist", "png", sep="."))
  pdf(paste(elements[i], "hist", "pdf", sep="."))
  hist(data_peaks_pos[,elements[i]], main=elements[i], xlab="", ylab="")#probability = TRUE,
  title(ylab="Frequency", mgp=c(2.5,1,0),cex.lab=1)
  title(xlab="LIBS Intensities", mgp=c(2.5,1,0),cex.lab=1)
  graphics.off()
  #dev.off()
}

## ALR Transformation
data_peaks_temp <- data_peaks_pos[,4:14]/data_peaks_pos[,15]
data_peaks_t <- data.frame(data_peaks_pos[,1:3], data_peaks_temp)
data_peaks_t[,4:14] <- log(data_peaks_t[,4:14])

elements_v2 <- colnames(data_peaks_t)[4:ncol(data_peaks_t)]

for(i in 1:length(elements_v2)){
  #png(paste(elements_v2[i], "log-hist", "png", sep="."))
  pdf(paste(elements_v2[i], "log-hist", "pdf", sep="."))
  hist(data_peaks_t[,elements_v2[i]], main=elements_v2[i], xlab="", ylab="")#probability = TRUE,
  title(ylab="Frequency", mgp=c(2.5,1,0),cex.lab=1)
  title(xlab="ALR Transformed Intensities (LIBS)", mgp=c(2.5,1,0),cex.lab=1)
  graphics.off()
  #dev.off()
}

## PCA
prcomp_glass_temp <- prcomp(data_peaks_t[,4:14], retx=TRUE)
#Importance of components:
#                       PC1     PC2     PC3     PC4     PC5     PC6     PC7     PC8
#Standard deviation     4.411 1.42624 0.90467 0.55954 0.49137 0.29095 0.23734 0.16187
#Proportion of Variance 0.844 0.08825 0.03551 0.01358 0.01047 0.00367 0.00244 0.00114
#Cumulative Proportion  0.844 0.93223 0.96774 0.98132 0.99179 0.99547 0.99791 0.99905
#                       PC9    PC10    PC11
#Standard deviation     0.12574 0.07160 0.03195
#Proportion of Variance 0.00069 0.00022 0.00004
#Cumulative Proportion  0.99973 0.99996 1.00000
prcomp_glass <- data_peaks_t[,1:3]
prcomp_glass <- cbind(prcomp_glass, prcomp_glass_temp$x)

## Remove the items with less observations than the others (98, 166)
aggregate_items <- aggregate(prcomp_glass[,3], by=list(prcomp_glass[,2]), FUN="max")
items_rem <- aggregate_items$Group.1[which(aggregate_items$x < max(aggregate_items$x))]
prcomp_glass_f <- prcomp_glass[! prcomp_glass$Item %in% items_rem, ]

prcomp_glass_f$Item2 <- rep(1:length(unique(prcomp_glass_f$Item)), each=length(unique(prcomp_glass_f$Piece)))
prcomp_glass_f$Item <- prcomp_glass_f$Item2
prcomp_glass_f <- prcomp_glass_f[, ! colnames(prcomp_glass_f) %in% "Item2"]

write.csv(prcomp_glass_f, "glassdata_v1_pr.csv", row.names = FALSE)
