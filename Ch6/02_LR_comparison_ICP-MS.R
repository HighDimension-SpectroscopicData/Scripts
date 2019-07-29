library('RPEnsemble')
library('comparison')

source("lr_fn.R")

data_train <- read.table('training.txt', header=TRUE)
data_control <- read.table('triplo.txt', header = TRUE)
data_recovered <- read.table('duplo.txt', header = TRUE)

m.all.analysed <- length(unique(data_control$Item))

data_train_obs <- as.matrix(data_train[,3:12])
data_control_obs <- as.matrix(data_control[,3:12])
data_recovered_obs <- as.matrix(data_recovered[,3:12])

variables <- c(3:5)

means_vec_data_train <- data.frame(matrix(0, nrow=100, ncol=length(variables)))

lr.Nor = matrix(0,ncol = m.all.analysed, nrow = m.all.analysed)
rownames(lr.Nor) = as.character(unique(data_control$Item))
colnames(lr.Nor) = as.character(unique(data_control$Item))

lr.KDE = matrix(0,ncol = m.all.analysed, nrow = m.all.analysed)
rownames(lr.KDE) = as.character(unique(data_control$Item))
colnames(lr.KDE) = as.character(unique(data_control$Item))

start.time <- Sys.time()
for(i in 1:100){
  rp <- RPGenerate(p=10, d=length(variables), method="Gaussian", B2=1) 
  
  data_train_rp <- data_train_obs %*% rp
  data_control_rp <- data_control_obs %*% rp
  data_recovered_rp <- data_recovered_obs %*% rp
  
  means_vec_data_train[i,] <- colMeans(data_train_rp)
  data_train_rp <- data.frame("item" = data_train$Item, "rep" = data_train$Piece, data_train_rp)
  data_control_rp <- data.frame("item" = data_control$Item, "rep" = data_control$Piece, data_control_rp)
  data_recovered_rp <- data.frame("item" = data_recovered$Item, "rep" = data_recovered$Piece, data_recovered_rp)
  
  lr.matrices <- lr(data_train_rp, data_control_rp, data_recovered_rp, variables, m.all.analysed)
  
  lr.Nor <- lr.Nor + lr.matrices$output.matrix.Nor
  lr.KDE <- lr.KDE + lr.matrices$output.matrix.KDE
}
end.time <- Sys.time()
(time.taken <- end.time - start.time)

lr.Nor <- lr.Nor/100
lr.KDE <- lr.KDE/100

write.table((lr.Nor), file = paste(length(variables),"comparison_research_Nor.txt", sep=""), quote = FALSE, sep = "\t", col.names = TRUE, row.names = TRUE, dec = ".", append = TRUE)
write.table((lr.KDE), file = paste(length(variables),"comparison_research_KDE.txt", sep=""), quote = FALSE, sep = "\t", col.names = TRUE, row.names = TRUE, dec = ".", append = TRUE)

##This part of code transforms the way the results are displayed (from an m x m matrix to one column of results)

fp.all = m.all.analysed*(m.all.analysed-1)/2 ##number of all possible results for different objects comparisons
lr.different.Nor = matrix(0, nrow=fp.all, ncol=1)
lr.different.KDE = matrix(0, nrow=fp.all, ncol=1)
q=0

for(s in 1:(m.all.analysed-1))
{
  for(r in (s+1):m.all.analysed)
  {
    q = 1 + q
    lr.different.Nor[q,1] = lr.Nor[r,s]
    lr.different.KDE[q,1] = lr.KDE[r,s]
  }
}

write.table((lr.different.Nor), file = paste(length(variables),"_comparison_research_Nor_different.txt", sep=""), quote = FALSE, sep = " ", col.names = TRUE, row.names = TRUE, dec = ".")
write.table((lr.different.KDE), file = paste(length(variables),"_comparison_research_KDE_different.txt", sep=""), quote = FALSE, sep = " ", col.names = TRUE, row.names = TRUE, dec = ".")

lr.same.Nor = matrix(0, nrow=m.all.analysed, ncol=1)
lr.same.KDE = matrix(0, nrow=m.all.analysed, ncol=1)

for(t in 1:m.all.analysed)
{
  lr.same.Nor[t,1] = lr.Nor[t,t]
  lr.same.KDE[t,1] = lr.KDE[t,t]
}

write.table(lr.same.Nor, file = paste(length(variables),"_comparison_research_Nor_same.txt", sep=""), quote = FALSE, sep = "\t", col.names = TRUE, row.names = TRUE, dec = ".")
write.table(lr.same.KDE, file = paste(length(variables),"_comparison_research_KDE_same.txt", sep=""), quote = FALSE, sep = "\t", col.names = TRUE, row.names = TRUE, dec = ".")

##Error Rates
fp.Nor = 0
fp.KDE = 0
fn.Nor = 0
fn.KDE = 0

fp.Nor <- length(which(lr.different.Nor > 1))*100/fp.all
fp.KDE <- length(which(lr.different.KDE > 1))*100/fp.all
fn.Nor <- length(which(lr.same.Nor < 1))*100/m.all.analysed
fn.KDE <- length(which(lr.same.KDE < 1))*100/m.all.analysed

error_rates = matrix(0, nrow = 4, ncol = 1)
rownames(error_rates) = c("fp_Nor", "fp_KDE", "fn_Nor", "fn_KDE")
colnames(error_rates) = paste("RP", length(variables), sep="_")
error_rates[1,1] = fp.Nor
error_rates[2,1] = fp.KDE
error_rates[3,1] = fn.Nor
error_rates[4,1] = fn.KDE
write.table(signif(error_rates, digits = 3), file = "comparison_research_error_rate.txt", append = TRUE, quote = FALSE, sep = "\t", col.names = TRUE, row.names = TRUE, dec = ".")
