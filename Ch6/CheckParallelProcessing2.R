## Check how different RPs are behaving in terms of LRs
## Use individual files for each RP

numfiles <- 1:100
namefiles <- paste(numfiles, "lr_mat", "txt", sep=".")
pathfiles <- paste("./Parallel Processing/", length(numfiles), "RPs/", namefiles, sep="")

m.all.analysed = 320
fp.all = m.all.analysed*(m.all.analysed-1)/2 ##number of all possible results for different objects comparisons
variables <- c(3:5)

lr.different.Nor = matrix(0, nrow=fp.all, ncol=length(numfiles))
lr.same.Nor = matrix(0, nrow=m.all.analysed, ncol=length(numfiles))

for(i in 1:length(numfiles)){
  
  temp <- read.table(pathfiles[i])
  
  ##This part of code transforms the way the results are displayed (from an m x m matrix to one column of results)

  lr.different.Nor_temp = matrix(0, nrow=fp.all, ncol=1)

  q=0
  
  for(s in 1:(m.all.analysed-1))
  {
    for(r in (s+1):m.all.analysed)
    {
      q = 1 + q
      lr.different.Nor_temp[q,1] = temp[r,s]
    }
  }
  
  lr.same.Nor_temp = matrix(0, nrow=m.all.analysed, ncol=1)
  
  for(t in 1:m.all.analysed)
  {
    lr.same.Nor_temp[t,1] = temp[t,t]
  }
  
  lr.different.Nor[,i] <- lr.different.Nor_temp
  lr.same.Nor[,i] <- lr.same.Nor_temp
}

lr.different.Nor <- cbind(1:fp.all, lr.different.Nor)
lr.same.Nor <- cbind(1:m.all.analysed, lr.same.Nor)

write.table(lr.same.Nor, paste("./Parallel Processing/", length(numfiles), "RPs/","lr.same.Nor.txt", sep = ""), row.names = FALSE)
write.table(lr.different.Nor, paste("./Parallel Processing/", length(numfiles), "RPs/","lr.different.Nor.txt", sep = ""), row.names = FALSE)

## Load libraries
library(MASS)

## Read the above txt files for plots
numfiles <- 1:100
lr.same.Nor <- read.table(paste("./Parallel Processing/", length(numfiles), "RPs/","lr.same.Nor.txt", sep = ""), header=TRUE)
lr.different.Nor <- read.table(paste("./Parallel Processing/", length(numfiles), "RPs/","lr.different.Nor.txt", sep = ""), header=TRUE)

lr.same.Nor_mat <- as.matrix(lr.same.Nor[,2:ncol(lr.same.Nor)])
lr.different.Nor_mat <- as.matrix(lr.different.Nor[,2:ncol(lr.different.Nor)])

## Histogram of LRs-same source comparison
minLR.same <- min(lr.same.Nor_mat)
maxLR.same <- max(lr.same.Nor_mat)
truehist(log10(lr.same.Nor_mat), main = "Random Projections 100: Same objects comparison")

## Histogram of LRs-different source comparison
minLR.diff <- min(lr.different.Nor_mat)
maxLR.diff <- max(lr.different.Nor_mat)
# Bin all LRs < 10^(-10) to 10^(-10)
lr.different.Nor_mat <- ifelse(lr.different.Nor_mat <= 10^(-10), 10^(-10), lr.different.Nor_mat)

par(mfrow=c(1,2))
truehist(log10(lr.different.Nor_mat), main = "Random Projections 100: Different objects comparison")
truehist(log10(lr.different.Nor_mat[lr.different.Nor_mat > 10^(-10)]), main = "Random Projections 100: Different objects comparison")
dev.off()

hist(log10(lr.different.Nor_mat[lr.different.Nor_mat > 10^(-10)]), probability = TRUE)
hist(log10(lr.different.Nor_mat), prob=TRUE)
plot(density(log10(lr.different.Nor_mat)))#main = "Density: Different objects comparison: Mean LR 100 RP"
plot(density(log10(lr.different.Nor_mat[lr.different.Nor_mat > 10^(-10)])))#main = "Density: Different objects comparison: Mean LR 100 RP"

## Check false positive, false negative error rates
lr.same.Nor_mean <- rowMeans(lr.same.Nor_mat)
fn <- length(which(lr.same.Nor_mean < 1))*100/length(lr.same.Nor_mean)
plot(density(log10(lr.same.Nor_mean)), main = "Density: Same objects comparison: Mean LR 100 RP")

lr.different.Nor_mean <- rowMeans(lr.different.Nor_mat)
fp <- length(which(lr.different.Nor_mean > 1))*100/length(lr.different.Nor_mean)
plot(density(log10(lr.different.Nor_mean)), main = "Density: Different objects comparison: Mean LR 100 RP")
abline(v = 0, lty = 2)

## Means of LRs may not be appropriate for this scenario because LR = 10^(-2) = 0.01, is strong evidence against proposition model.
## Simply taking mean considers this value very low, and if one of the random LR = 2, that takes the mean > 1.
## Try mean of log of LRs
lr.same.Nor_meanLog <- rowMeans(log10(lr.same.Nor_mat))
fn_logLR <- length(which(lr.same.Nor_meanLog < 0))*100/length(lr.same.Nor_meanLog) #0
plot(density((lr.same.Nor_meanLog)), main = "Density: Same objects comparison: Mean Log LR 100 RP")

lr.different.Nor_meanLog <- rowMeans(log10(lr.different.Nor_mat))
fp_logLR <- length(which(lr.different.Nor_meanLog > 0))*100/length(lr.different.Nor_meanLog) #0.186
par(mfrow=c(1,2))
plot(density((lr.different.Nor_meanLog)), main = "Density: Different objects comparison: Mean Log LR 100 RP")
plot(density((lr.different.Nor_meanLog[lr.different.Nor_meanLog > -10])), main = "Density: Different objects comparison: Mean Log LR[>-10] 100 RP")

## Time Series plot of LR.Same.Nor
minLR <- min(lr.same.Nor[,2:ncol(lr.same.Nor)])
#q3LR <- max(apply(lr.same.Nor[,2:ncol(lr.same.Nor)], 2, quantile)[4,])
maxLR <- max(lr.same.Nor[,2:ncol(lr.same.Nor)])
plot(1:m.all.analysed, lr.same.Nor[,2], type='p', ylim=c(minLR, maxLR))#q3LR+100000
points(1:m.all.analysed, lr.same.Nor[,3], col="red")
points(1:m.all.analysed, lr.same.Nor[,4], col="blue")
points(1:m.all.analysed, lr.same.Nor[,5], col="green")
points(1:m.all.analysed, lr.same.Nor[,6], col="orange")
s <- subset(lr.same.Nor, lr.same.Nor[,2] > 2.5*(10^19) | lr.same.Nor[,3] > 2.5*(10^19) | lr.same.Nor[,4] > 2.5*(10^19) |
            lr.same.Nor[,4] > 2.5*(10^19) | lr.same.Nor[,6] > 2.5*(10^19))
text(s[,2:6], labels = s[,1], pos = 2)
abline(h = 2.5*10^19)

## Take logarithm of LR.Same.Nor, and then plot
lr.same.Nor.log <- lr.same.Nor
lr.same.Nor.log[,2:6] <- log10(lr.same.Nor.log[,2:6])

minLR <- min(lr.same.Nor.log[,2:6])
q3LR <- max(apply(lr.same.Nor.log[,2:6], 2, quantile)[4,])
maxLR <- max(lr.same.Nor.log[,2:6])
plot(1:m.all.analysed, lr.same.Nor.log[,2], type='p', ylim=c(minLR, maxLR))#q3LR+100000
points(1:m.all.analysed, lr.same.Nor.log[,3], col="red")
points(1:m.all.analysed, lr.same.Nor.log[,4], col="blue")
points(1:m.all.analysed, lr.same.Nor.log[,5], col="green")
points(1:m.all.analysed, lr.same.Nor.log[,6], col="orange")

## Time Series plot of LR.Different.Nor
minLR <- min(lr.different.Nor[,2:6])
#q3LR <- max(apply(lr.different.Nor[,2:6], 2, quantile)[4,])
maxLR <- max(lr.different.Nor[,2:6])
plot(1:fp.all, lr.different.Nor[,2], type='p', ylim=c(minLR, maxLR))#q3LR+100000
points(1:fp.all, lr.different.Nor[,3], col="red")
points(1:fp.all, lr.different.Nor[,4], col="blue")
points(1:fp.all, lr.different.Nor[,5], col="green")
points(1:fp.all, lr.different.Nor[,6], col="orange")
s <- subset(lr.different.Nor, lr.different.Nor[,2] > 1*(10^7) | lr.different.Nor[,3] > 1*(10^7) | lr.different.Nor[,4] > 1*(10^7) |
              lr.different.Nor[,4] > 1*(10^7) | lr.different.Nor[,6] > 1*(10^7))
text(s[,2:6], labels = s[,1], pos = 2)

#lr.different.Nor.log <- lr.different.Nor
#lr.different.Nor.log[,2:6] <- log10(lr.different.Nor.log[,2:6])

## Remove the outlier observation and plot
lr.different.Nor_1 <- lr.different.Nor[-26565,]
minLR <- min(lr.different.Nor_1[,2:6])
maxLR <- max(lr.different.Nor_1[,2:6])
plot(1:(fp.all-1), lr.different.Nor_1[,2], type='p', ylim=c(minLR, maxLR))#q3LR+100000
points(1:(fp.all-1), lr.different.Nor_1[,3], col="red")
points(1:(fp.all-1), lr.different.Nor_1[,4], col="blue")
points(1:(fp.all-1), lr.different.Nor_1[,5], col="green")
points(1:(fp.all-1), lr.different.Nor_1[,6], col="orange")
s <- subset(lr.different.Nor_1, lr.different.Nor_1[,2] > (10^4) | lr.different.Nor_1[,3] > (10^4) | lr.different.Nor_1[,4] > (10^4) |
              lr.different.Nor_1[,4] > (10^4) | lr.different.Nor_1[,6] > (10^4))
text(s[,2:6], labels = s[,1], pos = 2)

## Remove the outlier observation and plot
lr.different.Nor_1 <- lr.different.Nor[-c(19414,26565),]
minLR <- min(lr.different.Nor_1[,2:6])
maxLR <- max(lr.different.Nor_1[,2:6])
plot(1:(fp.all-2), lr.different.Nor_1[,2], type='p', ylim=c(minLR, maxLR))#q3LR+100000
points(1:(fp.all-2), lr.different.Nor_1[,3], col="red")
points(1:(fp.all-2), lr.different.Nor_1[,4], col="blue")
points(1:(fp.all-2), lr.different.Nor_1[,5], col="green")
points(1:(fp.all-2), lr.different.Nor_1[,6], col="orange")
