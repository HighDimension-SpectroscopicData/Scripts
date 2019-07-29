Tippett_plot = function(LR.H1.exp, LR.H2.exp)
{
  log.LR.H1.exp = log10(LR.H1.exp)
  log.LR.H2.exp = log10(LR.H2.exp)
  
  log.LR.H1.exp[which(log.LR.H1.exp < -20),] = -20
  log.LR.H2.exp[which(log.LR.H2.exp < -20),] = -20
  log.LR.H1.exp[which(log.LR.H1.exp > 20),] = 20
  log.LR.H2.exp[which(log.LR.H2.exp > 20),] = 20
  
  min = min(log.LR.H1.exp,log.LR.H2.exp)
  max = max(log.LR.H1.exp,log.LR.H2.exp)
  
  x.range.data = rbind(min-1,log.LR.H1.exp,log.LR.H2.exp, max+1)
  x.range = x.range.data[order(x.range.data),1]
  
  Tippett.2 = matrix(0, nrow = length(x.range), ncol = 1)
  Tippett.1 = matrix(0, nrow = length(x.range), ncol = 1)
  
  for (i in 1:length(x.range))
  {
    Tippett.2[i] = length(which(log.LR.H2.exp > x.range[i]))/nrow(log.LR.H2.exp)*100
    Tippett.1[i] = length(which(log.LR.H1.exp > x.range[i]))/nrow(log.LR.H1.exp)*100
  }
  
  false.positives = round(length(which(log.LR.H2.exp > 0))/nrow(log.LR.H2.exp)*100,2)
  false.negatives = round(length(which(log.LR.H1.exp < 0))/nrow(log.LR.H1.exp)*100,2)
  tippett_col <- data.frame(x.range, Tippett.1, Tippett.2)
  tippett_col_fp <- tippett_col[tippett_col$x.range>0, c("x.range","Tippett.2")]
  tippett_col_fn <- tippett_col[tippett_col$x.range<0, c("x.range","Tippett.1")]
  
  plot(x.range, Tippett.2, type="s", xlab="", ylab="", xlim=c(-3,3), ylim=c(0,100), lty=3, lwd = 2)
  title(ylab="Proportion of cases [%]", mgp=c(2.5,1,0),cex.lab=1)
  title(xlab=expression(paste(log[10],"LR greater than")), mgp=c(2.5,1,0),cex.lab=1)
  par(new=TRUE)
  plot(x.range, Tippett.1, type="s", xlab="", ylab="", xlim=c(-3,3), ylim=c(0,100), lwd = 2)
  title(ylab="Proportion of cases [%]", mgp=c(2.5,1,0),cex.lab=1)
  title(xlab=expression(paste(log[10],"LR greater than")), mgp=c(2.5,1,0),cex.lab=1)
  legend("bottomleft", c(expression(paste("true-",H[2]," LR values")), expression(paste("true-",H[1]," LR values"))), lty=c(3,1), bty="n", lwd=c(2,2))
  abline(v=0, col="gray", lty=4)
  polygon(c(tippett_col_fp$x.range, rev(tippett_col_fp$x.range)), c(tippett_col_fp$Tippett.2, rep(0,length(tippett_col_fp$Tippett.2))), col="red")
  polygon(c(tippett_col_fn$x.range, rev(tippett_col_fn$x.range)), c(rep(100,length(tippett_col_fn$Tippett.1)), rev(tippett_col_fn$Tippett.1)), col="red")
}
