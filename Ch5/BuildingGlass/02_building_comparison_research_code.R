##Loading the functions needed for calculations
source("UC_comparison_calculations.R")
source("LR_comparison_Nor.R")
source("LR_comparison_KDE.R")

##Choosing the dataset ('data.analysed') on which calculations are carried out as well as the relevant database for parameters estimation ('population')
population = read.csv("glassdata_v1_pr.csv", header = TRUE) ##ENTER THE FILE NAME 
data.analysed = read.csv("glassdata_v1_pr.csv", header = TRUE) ##ENTER THE FILE NAME

##'m.all.analysed' corresponds to the number of objects on which calculations are carried out
m.all.analysed = length(unique(data.analysed$Item))

##In the 'variables' it is necessary to enter the index of the column in the univariate problem (or columns in the multivariate problem) for the relevant variable(s)
variables = c(4:6) ##ENTER THE NUMBER OF THE COLUMN(S) THE VARIABLE(S) IS (ARE) LOCATED IN, E.G. 4 FOR UNIVARIATE OR 4,5 FOR MULTIVARIATE MODELS
variables.names = colnames(data.analysed[variables])
variable.name = paste(variables.names, collapse = "_")
p = length(variables) ##number of variables considered; gives the idea of the problem dimensionality
n = length(unique(data.analysed$Piece)) ##number of measurements per object
fp.Nor = 0
fp.KDE = 0
fn.Nor = 0
fn.KDE = 0

##Defining two matrices of LR results which are to be saved in a .txt file
##The first is related to results assuming a normal distribution, the second to those when KDE is used
##The matrices are organised in such a way that all the LR values for each comparison may be found at the intersection of the row and column marked by the relevant item names
output.matrix.Nor = matrix(0,ncol = m.all.analysed, nrow = m.all.analysed)
rownames(output.matrix.Nor) = as.character(unique(data.analysed$Name))
colnames(output.matrix.Nor) = as.character(unique(data.analysed$Name))

output.matrix.KDE = matrix(0,ncol = m.all.analysed, nrow = m.all.analysed)
rownames(output.matrix.KDE) = as.character(unique(data.analysed$Name))
colnames(output.matrix.KDE) = as.character(unique(data.analysed$Name))

##AG EDIT3 - No Need for Jackknife procedure
#population = data.analysed ##choosing the relevant population by NOT employing the jackknife procedure
m = length(unique(population$Item)) ##number of objects creating a database
##'UC' function gives information about within- (U) and between- (C) object variability matrices 
results.UC = UC(population, variables, p, n)
U = results.UC$U
C = results.UC$C
mean.all = results.UC$mean.all ##mean of all measurements performed on all objects from a database

##'i' and 'j' indices run through all the compared items denoted by 'y.1' and 'y.2' with 'n.1' and 'n.2' measurements, respectively
##'i' is always smaller than or equal to 'j' so as to avoid repeating calculations
for (i in 1:m.all.analysed)#m.all.analysed
{  
	for (j in i:m.all.analysed) 
	{	
		if (i == j) ##When i = j, then the samples from the same object are compared (delivering the rates of false negative answers)
		{
		  y.1.2 = data.analysed[which(data.analysed$Item == i),] ##choosing an object described by n measurements which will be divided into two samples 'y.1' and 'y.2'
			y.1 = data.frame(y.1.2[1:3,]) ##choosing y.1 ##ENTER THE NUMBER OF THE ROW(S) CREATING THE CONTROL SAMPLE, E.G. 1:6
			y.2 = data.frame(y.1.2[4:6,]) ##choosing y.2 ##ENTER THE NUMBER OF THE ROW(S) CREATING THE RECOVERED SAMPLE, E.G. 7:12
			
			#population = data.analysed[which(data.analysed$item != i),] ##choosing the relevant population by employing the jackknife procedure
			#m = length(unique(population$item)) ##number of objects creating a database 
		}
		else ##When i is not equal to j, samples from different objects are compared (giving the false positive answer rates)
		{
		  #AG EDIT
		  #Instead of data.analysed, use population datasets. Since, population dataset has the principal components.
		  #data.analysed has the simulated data.
			y.1 = data.frame(data.analysed[which(data.analysed$Item == i),]) ##choosing 'y.1'
			y.2 = data.frame(data.analysed[which(data.analysed$Item == j),]) ##choosing 'y.2'

			#population = data.analysed[which(data.analysed$item != i),]
			#population = population[which(population$item != j),] ##choosing the relevant population by employing the jackknife procedure
			#m = length(unique(population$item)) ##number of objects creating a database
		}

		##'UC' function gives information about within- (U) and between- (C) object variability matrices 
		#results.UC = UC(population, variables, p, n) 
		#U = results.UC$U
		#C = results.UC$C
		#mean.all = results.UC$mean.all ##mean of all measurements performed on all objects from a database

		n.1 = length(y.1$Item) ##number of measurements performed on 'y.1'
		n.2 = length(y.2$Item) ##number of measurements performed on 'y.2'

		y.mean.1 = matrix(apply(as.matrix(y.1[,variables]), 2, mean), nrow = 1) ##mean for 'y.1'
		y.mean.2 = matrix(apply(as.matrix(y.2[,variables]), 2, mean), nrow = 1) ##mean for 'y.2'
		y.star = (n.1*y.mean.1+n.2*y.mean.2)/(n.1+n.2)    

		##Calculating smoothing parameter ('h') as a bandwidth for KDE procedure
		h = (4/(m*(2*p+1)))^(1/(p+4))

		##Calculating LR when between-object variability is assumed normal (denoted by 'Nor')
		results.LR.Nor = LR.Nor.function(y.mean.1, y.mean.2, y.star, mean.all, U, C, n.1, n.2, p)
		LR.Nor = results.LR.Nor$LR.Nor

		##Calculating the LR when between-object variability is estimated by KDE	
		results.LR.KDE= LR.KDE.function(y.mean.1, y.mean.2, y.star, U, C, h, population, variables, p, m, n.1, n.2)
		LR.KDE = results.LR.KDE$LR.KDE

		##Filling 'output matrix.Nor' and 'output.matrix.KDE' with LR results
		output.matrix.Nor[j,i] = LR.Nor
		output.matrix.KDE[j,i] = LR.KDE

		if(i != j & LR.Nor > 1) {fp.Nor = fp.Nor + 1} ##false positive answers (fp) when Nor is assumed
		if(i != j & LR.KDE > 1) {fp.KDE = fp.KDE + 1} ##false positive answers (fp) when KDE is used
		if(i == j & LR.Nor < 1) {fn.Nor = fn.Nor + 1} ##false negative answers (fn) when Nor is assumed
		if(i == j & LR.KDE < 1) {fn.KDE = fn.KDE + 1} ##false negative answers (fn) when KDE is used
	}	
}	

##Saving calculations' results to a file
write.table(signif(output.matrix.Nor, digits = 4), file = paste(variable.name,"comparison_research_Nor.txt", sep=""), quote = FALSE, sep = "\t", col.names = TRUE, row.names = TRUE, dec = ".", append = TRUE)
write.table(signif(output.matrix.KDE, digits = 4), file = paste(variable.name,"comparison_research_KDE.txt", sep=""), quote = FALSE, sep = "\t", col.names = TRUE, row.names = TRUE, dec = ".", append = TRUE)

##This part of code transforms the way the results are displayed (from an m x m matrix to one column of results)

fp.all = m.all.analysed*(m.all.analysed-1)/2 ##number of all possible results for different objects comparisons
LR.different.Nor = matrix(0, nrow=fp.all, ncol=1)
LR.different.KDE = matrix(0, nrow=fp.all, ncol=1)
q=0

for(s in 1:(m.all.analysed-1))
{
	for(r in (s+1):m.all.analysed)
	{
		q = 1 + q
		LR.different.Nor[q,1] = output.matrix.Nor[r,s]
		LR.different.KDE[q,1] = output.matrix.KDE[r,s]
	}
}
write.table(signif(LR.different.Nor, digits = 4), file = paste(variable.name,"_comparison_research_Nor_different.txt", sep=""), quote = FALSE, sep = " ", col.names = TRUE, row.names = TRUE, dec = ".")
write.table(signif(LR.different.KDE, digits = 4), file = paste(variable.name,"_comparison_research_KDE_different.txt", sep=""), quote = FALSE, sep = " ", col.names = TRUE, row.names = TRUE, dec = ".")

LR.same.Nor = matrix(0, nrow=m.all.analysed, ncol=1)
LR.same.KDE = matrix(0, nrow=m.all.analysed, ncol=1)

for(t in 1:m.all.analysed)
{
	LR.same.Nor[t,1] = output.matrix.Nor[t,t]
	LR.same.KDE[t,1] = output.matrix.KDE[t,t]
}
write.table(signif(LR.same.Nor, digits = 4), file = paste(variable.name,"_comparison_research_Nor_same.txt", sep=""), quote = FALSE, sep = "\t", col.names = TRUE, row.names = TRUE, dec = ".")
write.table(signif(LR.same.KDE, digits = 4), file = paste(variable.name,"_comparison_research_KDE_same.txt", sep=""), quote = FALSE, sep = "\t", col.names = TRUE, row.names = TRUE, dec = ".")

##Providing summary results expressed by levels of false positive and false negative model responses

error_rates = matrix(0, nrow = 4, ncol = 1)
rownames(error_rates) = c("fp_Nor", "fp_KDE", "fn_Nor", "fn_KDE")
colnames(error_rates) = variable.name
error_rates[1,1] = fp.Nor/fp.all*100
error_rates[2,1] = fp.KDE/fp.all*100
error_rates[3,1] = fn.Nor/m.all.analysed*100
error_rates[4,1] = fn.KDE/m.all.analysed*100
write.table(signif(error_rates, digits = 3), file = "comparison_research_error_rate.txt", append = TRUE, quote = FALSE, sep = "\t", col.names = TRUE, row.names = TRUE, dec = ".")

##Graphical presentation of descriptive statistics (box-plots, Q-Q plots, KDE density functions) and LR value distributions 
layout(matrix(c(rep(1,times=p),seq(2,(2*p+1))), 3, p, byrow=TRUE))

##Box-plots
boxplot(data.analysed[,variables], ylab = "data")

##Q-Q plots
for (k in 1:p) qqnorm(sapply(split(data.analysed[,variables[k]],data.analysed$Name),mean), main = variables.names[k])

##KDE probability density
#install.packages("KernSmooth")
require(KernSmooth)
p = 1
h = (4/(m.all.analysed*(2*p+1)))^(1/(p+4))
for (k in 1:length(variables))
plot(bkde(sapply(split(data.analysed[,variables[k]],data.analysed$Name),mean), kernel = "normal", bandwidth=h), type = "l", ylab = "probability density", xlab = "data", main = variables.names[k])

##Saving graphics to a .eps file
dev.copy(postscript, paste(variable.name,"_research_descriptive_statistics.eps", sep=""))
dev.off()

par(mfrow = c(2,2))

##Histograms illustrating LR distributions
hist(log10(LR.different.Nor), main = expression(paste("true-",H[d]," LR distribution assuming Nor", sep="")), col = "gray", xlab = "logLR", breaks = 50, cex.main = 0.7)
hist(log10(LR.different.KDE), main = expression(paste("true-",H[d]," LR distribution assuming KDE", sep="")), col = "gray", xlab = "logLR", breaks = 50, cex.main = 0.7)
hist(log10(LR.same.Nor), main = expression(paste("true-",H[p]," LR distribution assuming Nor", sep="")), col = "gray", xlab = "logLR", breaks = 50, cex.main = 0.7)
hist(log10(LR.same.KDE), main = expression(paste("true-",H[p]," LR distribution assuming KDE", sep="")), col = "gray", xlab = "logLR", breaks = 50, cex.main = 0.7)

##Saving graphics to a .eps file
dev.copy(postscript, paste(variable.name,"_research_LR_distribution.eps", sep=""))
dev.off()
