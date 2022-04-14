#Fonction calculant le quantile Q(t) pour la distribution de l'enonce
#Input:
#   t (float): variable de la fonction quantile Q(t)
#   theta_1,2 (float): parametres de la distribution
#Output:
#   float: quantile Q(t)

qdist<-function(t,theta_1,theta_2){
  result <- theta_2 * (1-t)^(-1/theta_1)
  return(result)
}

#Fonction permettant de generer un echantillon iid de taille n suivant
#la distribution de l'enonce
#Input:
#   smpl_size (int): taille de l'echantillon
#   theta_1,2 (float): parametres de la distribution
#Output:
#   vector of floats: echantillon iid (x1,x2,...,xn)
rdist<-function(smpl_size,theta_1,theta_2){
  U<-runif(smpl_size)
  result<-c()
  for(i in 1:smpl_size){
    result[i]<-qdist(U[i],theta_1,theta_2)
  }
  return(result)
}

#Fonction donnant la densite de probabilite de l'enonce
#Input:
#   x (float): variable de la dentite f(x)
#   theta_1,2 (float): parametres de la distribution
#Ouput:
#   float: densite au point x
ddist<-function(x,theta_1,theta_2){
  result<-theta_1 * (theta_2^theta_1)/(x^(theta_1+1))
  return(result)
}

#Exemples
t1<-10
t2<-0.3
n<-1e5
smpl<-rdist(n,t1,t2)
h<-hist(smpl,breaks=50,freq = FALSE)
xh<-h$mids
yh<-c()
for (i in 1:length(xh)){
  yh[i]<-ddist(xh[i],t1,t2)
}

lines(xh,yh)
abline(v=t2)