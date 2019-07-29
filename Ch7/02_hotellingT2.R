## Hotellings T2 Test
## R package - fdahotelling
## 1. Use fd objects from package fda
## 2. Use coefficients saved in fd objects from package fda

## Packages
library('fda')
library('fdahotelling')

## Read Data
mydata <- read.csv('dyes.csv', header=TRUE)

# Add another variable in the dataset
# Unique combination of dye color and sample number
id <- paste(mydata$color, mydata$sample, sep = "")
mydata <- cbind("name" = mydata[,1], id, mydata[,2:ncol(mydata)])

## Create bspline basis
basisfd <- create.bspline.basis(rangeval=c(1, 3201), nbasis=NULL, norder=5, breaks=seq(1, 3201, 20))
zero <- matrix(0, nrow = 1, ncol = 3201)
zero_t <- t(zero)
# Comparable fd object for zero
zero.fd <- Data2fd(zero_t, argvals = 1:3201, basisfd, fdnames=list("Wave Number","rep","Normalized Intensity"))
# Comparable fd coefficients for zero
zero.coef <- zero[,1:length(zero.fd$coefs)]

## 1. Use fd objects from package fda
## 2. Use coefficients saved in fd objects from package fda

# Matrix to save p-values for fd object comparison
pvals.fd <- matrix(NA, nrow=length(unique(id)), ncol=length(unique(id)))
rownames(pvals.fd) <- (unique(id))
colnames(pvals.fd) <- unique(id)

# Matrix to save Hotellings T2 statistic for fd object comparison
stat.fd <- matrix(NA, nrow=length(unique(id)), ncol=length(unique(id)))
rownames(stat.fd) <- (unique(id))
colnames(stat.fd) <- unique(id)

# Matrix to save p-values for fd coefficients comparison
pvals.coef <- matrix(NA, nrow=length(unique(id)), ncol=length(unique(id)))
rownames(pvals.coef) <- (unique(id))
colnames(pvals.coef) <- unique(id)

# Matrix to save Hotellings T2 statistic for fd coefficients comparison
stat.coef <- matrix(NA, nrow=length(unique(id)), ncol=length(unique(id)))
rownames(stat.coef) <- (unique(id))
colnames(stat.coef) <- unique(id)

for(i in 1:(length(unique(id)))){
  # y1: Control, y2: Recovered
  y1 <- t(mydata[mydata$id == unique(id)[i],6:3206])
  
  y1.fd <- Data2fd(y1, argvals = 1:3201, basisfd, fdnames=list("Wave Number","rep","Normalized Intensity"))
  y1.coef <- t(y1.fd$coefs)
  
  for(j in i:length(unique(id))){
    
    # Within Same Source Comparison
    if(i == j){
      n <- ncol(y1)
      n1 <- ifelse(n %% 2 == 0, n/2, (n+1)/2)
      
      y1.temp <- y1[,1:n1]
      y2.temp <- y1[,(n1+1):n]
    
      ctrl.fd <- Data2fd(y1.temp, argvals = 1:3201, basisfd, fdnames=list("Wave Number","rep","Normalized Intensity"))
      rcvd.fd <- Data2fd(y2.temp, argvals = 1:3201, basisfd, fdnames=list("Wave Number","rep","Normalized Intensity"))
      
      ctrl.coef <- t(ctrl.fd$coefs)
      rcvd.coef <- t(rcvd.fd$coefs)
      
    }else{
      ctrl.fd <- y1.fd
      ctrl.coef <- y1.coef
      
      y2 <- t(mydata[mydata$id == unique(id)[j],6:3206])
      
      rcvd.fd <- Data2fd(y2, argvals = 1:3201, basisfd, fdnames=list("Wave Number","rep","Normalized Intensity"))
      rcvd.coef <- t(rcvd.fd$coefs)
    }
    
    #test.fd <- test_twosample(x = ctrl.fd, y = rcvd.fd, step_size = 0.01, mu = zero.fd, B = 10000)
    test.coef <- test_twosample(x = ctrl.coef, y = rcvd.coef,  mu = zero.coef, B = 1000)#step_size = 0.01,
    
#    pvals.fd[j,i] <- test.fd$pValue
#    stat.fd[j,i] <- test.fd$statVal
    
    pvals.coef[j,i] <- test.coef$pValue
    stat.coef[j,i] <- test.coef$statVal
  }
}

#write.table(pvals.fd, "02_hotellingT2_output/B1000/pvals_FD.txt", quote=FALSE, sep="\t", col.names = TRUE, row.names = TRUE, dec = ".")
write.table(pvals.coef, "02_hotellingT2_output/B1000/pvals_Coef_stepsize0.txt", quote=FALSE, sep="\t", col.names = TRUE, row.names = TRUE, dec = ".")
#write.table(stat.fd, "02_hotellingT2_output/B1000/statistic_FD.txt", quote=FALSE, sep="\t", col.names = TRUE, row.names = TRUE, dec = ".")
write.table(stat.coef, "02_hotellingT2_output/B1000/statistic_Coefstepsize0.txt", quote=FALSE, sep="\t", col.names = TRUE, row.names = TRUE, dec = ".")
