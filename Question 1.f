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





#Fonction de génération d'un échantillon de taille 20 (plus simple pour définir la génération de 1000)

Gene <- function(i){
t1<-3
t2<-1
n<-20
smpl<-rdist(n,t1,t2)
}

#Génération d'un échantillon unique

Smpl1 <- Gene


#Répétition des 1000 fois de "Gene" (génération des 1000 échantillons de taille 20)
  
Smpl1000 <- t(sapply(1:1000,Gene))




#Traçage du plot comparatif(à mettre en place)

h<-hist(,breaks=50,freq = FALSE)
xh<-h$mids
yh<-c()
for (i in 1:length(xh)){
  yh[i]<-ddist(xh[i],t1,t2)
}


lines(xh,yh)
abline(v=t2)
