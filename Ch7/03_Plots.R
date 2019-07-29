mydata <- read.csv('dyes.csv', header=TRUE)

#Add another variable in the dataset
#Unique combination of dye color and sample number
id <- paste(mydata$color, mydata$sample, sep = "")
name <- mydata$name
mydata <- cbind("name" = mydata[,1], id, mydata[,2:ncol(mydata)])

#Separate out C, Y, M spectra
mydataC <- mydata[mydata$color == "C",]
mydataY <- mydata[mydata$color == "Y",]
mydataM <- mydata[mydata$color == "M",]

#Transpose the data
mydataC_t <- t(mydataC)
colnames(mydataC_t) <- mydataC_t[1,]
mydataC_t <- mydataC_t[- c(1:5),]
mydataC_t <- cbind(1:nrow(mydataC_t), mydataC_t)
class(mydataC_t) <- "numeric"

mydataY_t <- t(mydataY)
colnames(mydataY_t) <- mydataY_t[1,]
mydataY_t <- mydataY_t[- c(1:5),]
mydataY_t <- cbind(1:nrow(mydataY_t), mydataY_t)
class(mydataY_t) <- "numeric"

mydataM_t <- t(mydataM)
colnames(mydataM_t) <- mydataM_t[1,]
mydataM_t <- mydataM_t[- c(1:5),]
mydataM_t <- cbind(1:nrow(mydataM_t), mydataM_t)
class(mydataM_t) <- "numeric"

#C37, C452
pdf("C37_C452_v2.pdf", width = 8.27, height = 5.83)
plot(mydataC_t[,1:2], type='l',col="cyan", lwd = 1, main = "Functional Observations - Ink C37, C452", 
     xlab = "", ylab = "")
title(ylab="Absorbance", line=2.5, cex.lab=1)
title(xlab="Wavenumber", line=2.5, cex.lab=1)
for(i in 3:6){
  lines(mydataC_t[,1],mydataC_t[,i], col="cyan", lwd = 1)
}
for(i in 47:51){
  lines(mydataC_t[,1],mydataC_t[,i], col="cyan4", lwd = 1, lty = 6)
}
legend("topright", col = c("cyan", "cyan4"), lwd = 1, legend = c("C37", "C452"), bty="n", lty = c(1, 6))
graphics.off()

#M374, M413
plot(mydataM_t[,c(1, 27)], type='l',col="magenta", lwd = 2, main = "Functional Observations - Ink M374, M413", xlab = "Wavenumber", ylab = "Absorbance")
for(i in 28:31){
  lines(mydataM_t[,1],mydataM_t[,i], col="magenta", lwd = 2)
}
for(i in 37:41){
  lines(mydataM_t[,1],mydataM_t[,i], col="magenta4", lwd = 2)
}
legend("topright", col = c("magenta", "magenta4"), lwd = 2, legend = c("M374", "M413"), bty="n")

#---------------

#C37, C298
plot(mydataC_t[,1:2], type='l',col="cyan", lwd = 2, main = "Functional Observations - Ink C37, C298", xlab = "Wavenumber", ylab = "Absorbance")
for(i in 3:6){
  lines(mydataC_t[,1],mydataC_t[,i], col="cyan", lwd = 2)
}
for(i in 12:16){
  lines(mydataC_t[,1],mydataC_t[,i], col="cyan3", lwd = 2)
}
legend("topright", col = c("cyan", "cyan3"), lwd = 2, legend = c("C37", "C298"), bty="n")

#C374, C452
plot(mydataC_t[,c(1,27)], type='l',col="cyan", lwd = 2, main = "Functional Observations - Ink C374, C452", xlab = "Wavenumber", ylab = "Absorbance")
for(i in 28:31){
  lines(mydataC_t[,1],mydataC_t[,i], col="cyan", lwd = 2)
}
for(i in 47:51){
  lines(mydataC_t[,1],mydataC_t[,i], col="cyan3", lwd = 2)
}
legend("topright", col = c("cyan", "cyan3"), lwd = 2, legend = c("C374", "C452"), bty="n")

#C373, C452
plot(mydataC_t[,c(1,22)], type='l',col="cyan", lwd = 2, main = "Functional Observations - Ink C373, C452", xlab = "Wavenumber", ylab = "Absorbance")
for(i in 23:26){
  lines(mydataC_t[,1],mydataC_t[,i], col="cyan", lwd = 2)
}
for(i in 47:51){
  lines(mydataC_t[,1],mydataC_t[,i], col="cyan3", lwd = 2)
}
legend("topright", col = c("cyan", "cyan3"), lwd = 2, legend = c("C373", "C452"), bty="n")

#C372, C440
pdf("C372_C440_v2.pdf", width = 8.27, height = 5.83)
plot(mydataC_t[,c(1,17)], type='l',col="cyan", lwd = 1, main = "Functional Observations - Ink C372, C440", 
     xlab = "", ylab = "")
title(ylab="Absorbance", line=2.5, cex.lab=1)
title(xlab="Wavenumber", line=2.5, cex.lab=1)
for(i in 18:21){
  lines(mydataC_t[,1],mydataC_t[,i], col="cyan", lwd = 1)
}
for(i in 42:46){
  lines(mydataC_t[,1],mydataC_t[,i], col="cyan4", lwd = 1, lty = 6)
}
legend("topright", col = c("cyan", "cyan4"), lwd = 1, legend = c("C372", "C440"), bty="n", lty = c(1, 6))
graphics.off()

#Y37, Y452
pdf("Y37_Y452_v2.pdf", width = 8.27, height = 5.83)
plot(mydataC_t[,c(1,2)], type='l',col="goldenrod1", lwd = 1, main = "Functional Observations - Ink Y37, Y452", 
     xlab = "", ylab = "")
title(ylab="Absorbance", line=2.5, cex.lab=1)
title(xlab="Wavenumber", line=2.5, cex.lab=1)
for(i in 3:6){
  lines(mydataC_t[,1],mydataC_t[,i], col="goldenrod1", lwd = 1)
}
for(i in 46:50){
  lines(mydataC_t[,1],mydataC_t[,i], col="gold3", lwd = 1, lty = 6)
}
legend("topright", col = c("goldenrod1", "gold3"), lwd = 1, legend = c("Y37", "Y452"), bty="n", lty = c(1, 6))
graphics.off()