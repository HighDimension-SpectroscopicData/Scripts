histograms = function(LR.H1.exp, LR.H2.exp)
{
  log.LR.H1.exp = log10(LR.H1.exp)
  log.LR.H2.exp = log10(LR.H2.exp)
  
  log.LR.H1.exp[which(log.LR.H1.exp < -20),] = -20
  log.LR.H2.exp[which(log.LR.H2.exp < -20),] = -20
  log.LR.H1.exp[which(log.LR.H1.exp > 20),] = 20
  log.LR.H2.exp[which(log.LR.H2.exp > 20),] = 20
  
  min = min(log.LR.H1.exp,log.LR.H2.exp)
  max = max(log.LR.H1.exp,log.LR.H2.exp)
  breaks = 50
  stepbins = (max-min)/breaks
  xbars = seq(min, max, by=stepbins)
  
  log.LR.H1.exp = log.LR.H1.exp[(log.LR.H1.exp < (xbars[length(xbars)]-stepbins/2)) & log.LR.H1.exp > (xbars[1]-stepbins/2)]
  log.LR.H2.exp = log.LR.H2.exp[(log.LR.H2.exp < (xbars[length(xbars)]-stepbins/2)) & log.LR.H2.exp > (xbars[1]-stepbins/2)]
  
  xlim <- seq(min, max, by = 2.5)
  
  if(max %% 2.5 != 0){
    max = xlim[length(xlim)]+ 2.5
  }
  
  
  set = par(mfrow=c(2,1), mar=c(4,4,1,2))
  hist(log.LR.H1.exp, breaks=xbars-stepbins/2, col="blue", main="", xlab="", ylab="", axes = FALSE)#, xlim = c(min, max)
  axis(1, at = seq(min, max, by = 2.5), labels = seq(min, max, by = 2.5))
  axis(2)
  title(ylab="Frequency", mgp=c(2.5,1,0),cex.lab=1)
  title(xlab="logLR", mgp=c(2.5,1,0),cex.lab=1)
  legend("topleft", expression(paste(H[1]," true")), fill="blue")
  abline(v = 0, lty = 2, lwd = 2)
  
  hist(log.LR.H2.exp, breaks=xbars-stepbins/2, col="darkorange", main="", xlab="", ylab="", axes = FALSE)
  axis(1, at = seq(min, max, by = 2.5), labels = seq(min, max, by = 2.5))
  axis(2)  
  title(ylab="Frequency", mgp=c(2.5,1,0),cex.lab=1)
  title(xlab="logLR", mgp=c(2.5,1,0),cex.lab=1)
  legend("topright", expression(paste(H[2]," true")), fill="darkorange")
  abline(v = 0, lty = 2, lwd = 2)
  
  par(set)
}