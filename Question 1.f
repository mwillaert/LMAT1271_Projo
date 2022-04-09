#Question 1.f

#Préambule
#Recycler le code de la question e, mais répéter 1000fois le processus

qdist<-function(t,theta_1,theta_2){
  result <- theta_2 * (1-t)^(-1/theta_1)
  return(result)
}

rdist<-function(smpl_size,theta_1,theta_2){
  U<-runif(smpl_size)
  result<-c()
  for(i in 1:smpl_size){
    result[i]<-qdist(U[i],theta_1,theta_2)
  }
  return(result)
}

ddist<-function(x,theta_1,theta_2){
  result<-theta_1 * (theta_2^theta_1)/(x^(theta_1+1))
  return(result)
}



#Répétition des 1000 fois

count<--1

repeat{
t1<-3
t2<-1
n<-20
smpl<-rdist(n,t1,t2)
count=count+1
if(count==1000){break}
}

h<-hist(smpl,breaks=50,freq = FALSE)
xh<-h$mids
yh<-c()
for (i in 1:length(xh)){
  yh[i]<-ddist(xh[i],t1,t2)
}


#Traçage du plot comparatif


lines(xh,yh)
abline(v=t2)
