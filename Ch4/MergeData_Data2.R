#Create a dataset with observations from Peaks_Original.csv (Data.xlsx), Peaks_Original_Data2.csv (Data2.xlsx)

peaks1 <- read.csv('Peaks_Original.csv', header = TRUE)
peaks2 <- read.csv('Peaks_Original_Data2.csv', header = TRUE)

#All Inf values for Sr407 in peaks2. Replace these with the minimum intensity recorded for Sr407 in peaks1.

sr407_min <- min(peaks1[,'Sr.407.Avg'])

peaks2$Sr.407.Avg <- sr407_min

peaks <- rbind(peaks1, peaks2)

write.csv(peaks, 'Peaks_Original_Combined.csv', row.names = FALSE)
