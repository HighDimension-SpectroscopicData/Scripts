

setwd("~/Documents/Anjali Gupta/University of Auckland/Research/ESR/2.1Experiment-PostGrad")

install.packages('xlsx')
library('xlsx')

wb <- loadWorkbook("Data2.xlsx")

colnumbers <- c(1, seq(9,42,3))
data_peaks <- data.frame()

data_peaks <- read.xlsx("Data2.xlsx", sheetIndex = 1, startRow = 11, header=TRUE)
data_peaks <- data_peaks[,colnumbers]

colnames(data_peaks)[1] <- "Size"

## Replace negative observations with the minimum intensity recorded for that element
peaks_w_neg <- apply(data_peaks[,2:ncol(data_peaks)], 2, min)
cols_w_neg <- which(peaks_w_neg < 0) + 1

data_peaks_pos <- data_peaks

for(i in 1:length(cols_w_neg)){
  cols_replace <- min(data_peaks[which(data_peaks[,cols_w_neg[i]] >= 0), cols_w_neg[i]])
  data_peaks_pos[,cols_w_neg[i]] <- ifelse(data_peaks_pos[,cols_w_neg[i]]<0, cols_replace, data_peaks_pos[,cols_w_neg[i]])
} #Warning at this stage as Sr407 has all negatives for Data2.xlsx. Now this column has 'Inf' after running the above piece of code.
#Proceed as is, at the time when this has to be appended with the other data, make sure to replace these Inf observations with the
#minimum of the intensity recorded in that column for that data.

## Convert 1st column to numeric by removing the unit - mm
data_peaks_pos$Size_mm <- unlist(strsplit(as.character(data_peaks_pos[,1]), "mm"))

data_peaks_pos2 <- data_peaks_pos[,c(14, 2:13)]

#write.csv(data_peaks_pos2, "Peaks_Original.csv", row.names = FALSE)
write.csv(data_peaks_pos2, "Peaks_Original_Data2.csv", row.names = FALSE)
