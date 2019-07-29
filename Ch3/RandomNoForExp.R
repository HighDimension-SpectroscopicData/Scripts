#Four Types of Samples: Control Glass (C), Building (B), Vehicle (V), Bottle Glass (Bt)
#Four different samples for each type
#Around three runs for each glass sample each day
#Experiment run for 4 days
#Initially, Total 4*4*3*5 runs = 240 runs
#Finally, Total 4(Types of Samples)*3(Samples from each type)*3(Runs)*4(days) = 144

days <- rep(1:4, each=36)
#treatments <- rep(1:4, 15)
replications_eachday <- rep(1:3, each=12)
replications <- rep(replications_eachday, 4)

order <- rep(NA, 144)
for(i in 1:4){
  for(j in 1:3){
    order[((i-1)*36+(j-1)*12+1):((i-1)*36+(j-1)*12+12)] <- sample(1:12)
  }
}

intensities <- data.frame(days, replications, order)
write.csv(intensities, file = "RandomNoForExp.csv", row.names=FALSE)
