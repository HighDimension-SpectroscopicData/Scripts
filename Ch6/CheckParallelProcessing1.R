## Check if averaging in parallel processing is working as it should
## Use the individual files saved for all 100 RPs

numfiles <- 1:6
namefiles <- paste(numfiles, "lr_mat", "txt", sep=".")
pathfiles <- paste("./Parallel Processing/Mean Log LR/6RPs", namefiles, sep="/")

sumfiles <- matrix(0.0, nrow=320, ncol=320)
m.all.analysed = 320
variables <- c(3:5)

for(i in 1:length(numfiles)){
  
  temp <- as.matrix(read.table(pathfiles[i]))
  
  temp[lower.tri(temp, diag = TRUE) & temp <= 10^(-323)] <- 10^(-323)
  
  logtemp <- temp
  logtemp[lower.tri(logtemp, diag = TRUE)] <-  log10(logtemp[lower.tri(logtemp, diag = TRUE)])
  
  sumfiles <- sumfiles + logtemp
}

lr.Nor <- sumfiles/6

##This part of code transforms the way the results are displayed (from an m x m matrix to one column of results)

fp.all = m.all.analysed*(m.all.analysed-1)/2 ##number of all possible results for different objects comparisons
lr.different.Nor = matrix(0, nrow=fp.all, ncol=1)
#lr.different.KDE = matrix(0, nrow=fp.all, ncol=1)
q=0

for(s in 1:(m.all.analysed-1))
{
  for(r in (s+1):m.all.analysed)
  {
    q = 1 + q
    lr.different.Nor[q,1] = lr.Nor[r,s]
    #    lr.different.KDE[q,1] = lr.KDE[r,s]
  }
}

write.table((lr.different.Nor), file = paste(length(variables),"_comparison_research_Nor_different.txt", sep=""), quote = FALSE, sep = " ", col.names = TRUE, row.names = TRUE, dec = ".")
write.table((lr.different.KDE), file = paste(length(variables),"_comparison_research_KDE_different.txt", sep=""), quote = FALSE, sep = " ", col.names = TRUE, row.names = TRUE, dec = ".")

lr.same.Nor = matrix(0, nrow=m.all.analysed, ncol=1)
#lr.same.KDE = matrix(0, nrow=m.all.analysed, ncol=1)

for(t in 1:m.all.analysed)
{
  lr.same.Nor[t,1] = lr.Nor[t,t]
  #  lr.same.KDE[t,1] = lr.KDE[t,t]
}

write.table(lr.same.Nor, file = paste(length(variables),"_comparison_research_Nor_same.txt", sep=""), quote = FALSE, sep = "\t", col.names = TRUE, row.names = TRUE, dec = ".")
write.table(lr.same.KDE, file = paste(length(variables),"_comparison_research_KDE_same.txt", sep=""), quote = FALSE, sep = "\t", col.names = TRUE, row.names = TRUE, dec = ".")

##Error Rates
fp.Nor = 0
fp.KDE = 0
fn.Nor = 0
fn.KDE = 0

fp.Nor <- length(which(lr.different.Nor > 0))*100/fp.all
#fp.KDE <- length(which(lr.different.KDE > 1))*100/fp.all
fn.Nor <- length(which(lr.same.Nor < 0))*100/m.all.analysed
#fn.KDE <- length(which(lr.same.KDE < 1))*100/m.all.analysed

error_rates = matrix(0, nrow = 4, ncol = 1)
rownames(error_rates) = c("fp_Nor", "fp_KDE", "fn_Nor", "fn_KDE")
colnames(error_rates) = paste("RP", length(variables), sep="_")
error_rates[1,1] = fp.Nor
#error_rates[2,1] = fp.KDE
error_rates[3,1] = fn.Nor
#error_rates[4,1] = fn.KDE
write.table(signif(error_rates, digits = 3), file = "comparison_research_error_rate.txt", append = TRUE, quote = FALSE, sep = "\t", col.names = TRUE, row.names = TRUE, dec = ".")
