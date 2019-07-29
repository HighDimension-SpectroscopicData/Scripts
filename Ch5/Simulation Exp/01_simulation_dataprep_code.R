#setwd("~/Documents/Anjali Gupta/University of Auckland/Research/ESR/4.Experiment-AitkenLucy/Simulation Exp/Using scripts in Zadora's book/")

library(comparison)
library(mnormt)

data(glass)
data_glass <- as.data.frame(glass)

#PCA
prcomp_glass_temp <- prcomp(data_glass[,3:9], retx=TRUE)
summary(prcomp_glass_temp)
#Importance of components:
#                       PC1    PC2    PC3    PC4     PC5     PC6     PC7
#Standard deviation     1.9235 1.3977 1.1820 0.8389 0.72189 0.05114 0.04204
#Proportion of Variance 0.4469 0.2359 0.1687 0.0850 0.06294 0.00032 0.00021
#Cumulative Proportion  0.4469 0.6828 0.8515 0.9365 0.99947 0.99979 1.00000

#screeplot(prcomp_glass_temp, npcs = 7, main = "Scree Plot: Simulation Study", xlab = "Principal Component")
library(factoextra)
pdf("screeplot.pdf")
fviz_eig(prcomp_glass_temp, main = "Scree Plot: Simulation Study")
dev.off()

prcomp_glass <- as.data.frame(rep(1:200, each=12))
colnames(prcomp_glass)[1] <- "item"
prcomp_glass$rep <- rep(1:12, 200)
prcomp_glass[,3:9] <- prcomp_glass_temp$x
colnames(prcomp_glass)[3:9] <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7")

#Simulated Data
prcomp_glass <- prcomp_glass[! prcomp_glass$item %in% c(12,18,90,129,133),]

prcomp_glass$item2 <- rep(1:length(unique(prcomp_glass$item)), each=length(unique(prcomp_glass$rep)))
prcomp_glass$item <- prcomp_glass$item2
prcomp_glass <- prcomp_glass[, ! colnames(prcomp_glass) %in% "item2"]

data_prcomp_sim <- as.data.frame(rep(1:195, each=12))#195
data_prcomp_sim$rep <- rep(1:12, 195)#195
colnames(data_prcomp_sim)[1] <- "item"
data_prcomp_sim[,colnames(prcomp_glass)[3:9]] <- NA

for(i in 1:length(unique(prcomp_glass$item))){
  vcov_prcomp_temp <- two.level.components(prcomp_glass[prcomp_glass$item==i,], data.columns = c(3:9), item.column = 1)
  #eigen(vcov_prcomp_temp@v.within, only.values = FALSE)
  set.seed(1001)
  sim_temp <- rmnorm(n=12, mean=vcov_prcomp_temp@item.means, varcov=vcov_prcomp_temp@v.within)  
  data_prcomp_sim[data_prcomp_sim$item == i, 3:9] <- sim_temp
}

rm(sim_temp)
rm(vcov_prcomp_temp)

write.csv(prcomp_glass, "glassdata_v1_pr.csv", row.names = FALSE)
write.csv(data_prcomp_sim, "glassdata_v1_prsim.csv", row.names = FALSE)
