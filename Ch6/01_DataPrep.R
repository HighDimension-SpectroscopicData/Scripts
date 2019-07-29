data_train <- read.table('training.txt', header=TRUE)
data_control <- read.table('triplo.txt', header = TRUE)
data_recovered <- read.table('duplo.txt', header = TRUE)

summary(data_train)
## Summary of data_train shows that K39's concentration is the highest
## in terms of Maximum's. Even the Minimum values, only Ti49 > K39
data_train_mat <- data.matrix(data_train[,3:12])
cv <- function(x){
  return(sd(x)*100/mean(x))
}
cv_data_train_mat <- apply(data_train_mat, MARGIN = 2, FUN = cv)
#K39      Ti49      Mn55      Rb85      Sr88      Zr90     Ba137 
#11.940988  8.169179 15.821160 60.148527 11.693614 13.477447 19.713506 
#La139     Ce140     Pb208 
#39.965448 36.907804 56.518979 
## Coefficient of Variation is the least for Ti49, followed by K39
par(mfrow=c(1,2))
hist(data_train$Ti49)
hist(data_train$K39)

## Transform
## ALR transformation already done w.r.t Silicon

