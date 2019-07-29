## There are 2 files D3R3C3 - one for Day 3, one for Day 2
## It has been corrected for in file Data.xlsx - Sheet Day 2

setwd("~/Documents/Anjali Gupta/University of Auckland/Research/ESR/2.Experiment-PostGrad/Data")

install.packages('xlsx')
library('xlsx')

wb <- loadWorkbook("Data.xlsx")
sheets <- getSheets(wb)

colnumbers <- c(1, seq(9,42,3))
data_peaks <- data.frame()

for(i in 1:length(sheets)){
  temp <- read.xlsx("Data.xlsx", sheetIndex = i, startRow = 11, header=TRUE)
  temp <- temp[,colnumbers]
  data_peaks <- rbind(data_peaks, temp)
}

colnames(data_peaks)[1] <- "Name"

## Re-arrange dataframe based on the order in which experiments were conducted
order <- read.csv('RandomNoForExp_v2.csv', header=TRUE)
order_v2 <- order[,c("name", "days", "replications", "order", "samples")]

data_peaks_f <- merge(order_v2, data_peaks, by.x="name", by.y="Name", sort=FALSE)

## Replace negative observations with the minimum intensity recorded for that element
peaks_w_neg <- apply(data_peaks_f[,6:17], 2, min)
cols_w_neg <- which(peaks_w_neg < 0) + 5

data_peaks_pos <- data_peaks_f

for(i in 1:length(cols_w_neg)){
  cols_replace <- min(data_peaks_f[which(data_peaks_f[,cols_w_neg[i]] >= 0), cols_w_neg[i]])
  data_peaks_pos[,cols_w_neg[i]] <- ifelse(data_peaks_pos[,cols_w_neg[i]]<0, cols_replace, data_peaks_pos[,cols_w_neg[i]])
}

## Add a col for runs - 1-36 each day
runs <- rep(1:36, 4)
data_peaks_pos2 <- data_peaks_pos[,1:3]
data_peaks_pos2 <- cbind(data_peaks_pos2, runs)
data_peaks_pos2 <- cbind(data_peaks_pos2, data_peaks_pos[,4:17])

write.csv(data_peaks_pos2, "Peaks_Original_withOrder.csv", row.names = FALSE)
