days <- rep(1:5, each=12)
#treatments <- rep(1:4, 15)
#replications_eachday <- rep(1:3, each=4)
#replications <- rep(replications_eachday, 5)

order <- rep(NA, 60)
for(i in 1:5){
  for(j in 1:3){
    order[((i-1)*12+(j-1)*4+1):((i-1)*12+(j-1)*4+4)] <- sample(1:4)
  }
}

intensities <- data.frame(days, order)
write.csv(intensities, file = "C:/Users/agup122/University of Auckland/Research/ESR/RandomNoForExp.csv", row.names=FALSE)