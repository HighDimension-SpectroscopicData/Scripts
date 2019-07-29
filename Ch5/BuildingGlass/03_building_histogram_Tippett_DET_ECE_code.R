
source("histogram_function.R")
#source("ECE_function.R")
source("Tippett_function.R")
#source("DET_function.R")


##inserting the data
variables = c("PC1","PC1_PC2","PC1_PC2_PC3")
for (v in 1:length(variables))
{
  LR.H1.exp = read.table(paste(variables[v], "_comparison_research_Nor_same.txt", sep=""), header=TRUE)
  LR.H2.exp = read.table(paste(variables[v], "_comparison_research_Nor_different.txt", sep=""), header=TRUE)
  
  #For v = 1, change histogram_function to display legend on "topleft" for the second histogram
  pdf(paste(variables[v],"_Nor_histograms_v1.pdf", sep=""), width = 8.27, height = 5.83)
  histograms(LR.H1.exp, LR.H2.exp) ##plotting the histograms
  graphics.off()
  #dev.copy(postscript, paste(variables[v],"_Nor_histograms.eps", sep=""))
  #dev.off()
  
  pdf(paste(variables[v],"_Nor_Tippett.pdf", sep=""), width = 8.27, height = 5.83)
  Tippett_plot(LR.H1.exp, LR.H2.exp) ##plotting the Tippett plots
  graphics.off()
  #dev.copy(postscript, paste(variables[v],"_Nor_Tippett.eps", sep=""))
  #dev.off()
  
  #DET_plot(LR.H1.exp, LR.H2.exp) ##plotting the DET plots
  #dev.copy(postscript, paste(variables[v],"_Nor_DET.eps", sep=""))
  #dev.off()
  
  #ECE_plot(LR.H1.exp, LR.H2.exp) ##plotting the ECE curves
  #dev.copy(postscript, paste(variables[v],"_Nor_ECE.eps", sep=""))
  #dev.off()
    
}

for (v in 1:length(variables))
{
  LR.H1.exp = read.table(paste(variables[v], "_comparison_research_KDE_same.txt", sep=""), header=TRUE)
  LR.H2.exp = read.table(paste(variables[v], "_comparison_research_KDE_different.txt", sep=""), header=TRUE)
  
  histograms(LR.H1.exp, LR.H2.exp) ##plotting the histograms
  dev.copy(postscript, paste(variables[v],"_KDE_histograms.eps", sep=""))
  dev.off()
  
  Tippett_plot(LR.H1.exp, LR.H2.exp) ##plotting the Tippett plots
  dev.copy(postscript, paste(variables[v],"_KDE_Tippett.eps", sep=""))
  dev.off()
  
  DET_plot(LR.H1.exp, LR.H2.exp) ##plotting the DET plots
  dev.copy(postscript, paste(variables[v],"_KDE_DET.eps", sep=""))
  dev.off()
  
  ECE_plot(LR.H1.exp, LR.H2.exp) ##plotting the ECE curves
  dev.copy(postscript, paste(variables[v],"_KDE_ECE.eps", sep=""))
  dev.off()
  
}