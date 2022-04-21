best<-c(5,7,13,6,11,10,10,9,9,4,9,7,8,11,11,14,8,9,9,5,4,15,11,11,17,13,11,8,7,12,9,5,7,12,9,11,13,8,8,14,8,15,8,6,6,13,10,15,11,7,7,17,8,5,3,12,10,13,6,3,14,6,13,15,7)
hb<-hist(best,freq=FALSE)
plot(hb,xlim=c(0,25),main="Distribution of best South Park episodes",xlab="Seasons")