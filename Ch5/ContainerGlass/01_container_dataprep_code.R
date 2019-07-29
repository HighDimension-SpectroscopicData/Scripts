setwd("~/Documents/Anjali Gupta/University of Auckland/Research/ESR/4.Experiment-AitkenLucy/ContainerGlass/Using scripts in Zadora's book")

data_peaks <- read.csv("CG_Peaks_Original.csv", header = TRUE)

## Replacing negative observations with the minimum observation recorded for the peak
peaks_w_neg <- apply(data_peaks[,4:15], 2, min)
cols_w_neg <- which(peaks_w_neg < 0) + 3

data_peaks_pos <- data_peaks

for(i in 1:length(cols_w_neg)){
  cols_replace <- min(data_peaks[which(data_peaks[,cols_w_neg[i]] >= 0), cols_w_neg[i]])
  data_peaks_pos[,cols_w_neg[i]] <- ifelse(data_peaks_pos[,cols_w_neg[i]]<0, cols_replace, data_peaks_pos[,cols_w_neg[i]])
}

## ALR Transformation
data_peaks_temp <- data_peaks_pos[,4:14]/data_peaks_pos[,15]
data_peaks_t <- data.frame(data_peaks_pos[,1:3], data_peaks_temp)
data_peaks_t[,4:14] <- log(data_peaks_t[,4:14])

## PCA
prcomp_glass_temp <- prcomp(data_peaks_t[,4:14], retx=TRUE)
#Importance of components:
#                       PC1    PC2    PC3    PC4     PC5     PC6     PC7    PC8
#Standard deviation     4.3568 2.8235 2.3634 2.2602 1.65719 0.85752 0.65337 0.4489
#Proportion of Variance 0.4519 0.1898 0.1330 0.1216 0.06539 0.01751 0.01016 0.0048
#Cumulative Proportion  0.4519 0.6417 0.7747 0.8963 0.96174 0.97925 0.98941 0.9942
#                       PC9    PC10    PC11
#Standard deviation     0.32632 0.28799 0.23206
#Proportion of Variance 0.00254 0.00197 0.00128
#Cumulative Proportion  0.99674 0.99872 1.00000
prcomp_glass <- data_peaks_t[,1:3]
prcomp_glass <- cbind(prcomp_glass, prcomp_glass_temp$x)

prcomp_glass_f <- prcomp_glass

write.csv(prcomp_glass_f, "glassdata_v1_pr.csv", row.names = FALSE)
