#Generating the sample

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

#Estimators

#Estimateurs MLE de theta_1,2
#Input:
#   smpl (vector of floats): echantillon genere par theta
#Output:
#   vector of floats: estimateurs MLE (t_1,t_2) de theta_1,theta_2

theta_MLE<-function(smpl){
  t2<-min(smpl)
  #moyenne geometrique de l'echantillon
  gm<-exp(mean(log(smpl)))
  t1<- 1/log(gm/t2)
  return(c(t1,t2))
}

#Estimateurs MME

#Estimateurs MME de theta_1,2
#Input:
#   smpl (vector of floats): echantillon genere selon dist enonce
#Output:
#   vector of floats: estimateurs MLE (t_1,t_2) de theta_1,theta_2

theta_MME<-function(smpl){
  m1<-mean(smpl)
  m2<-mean(smpl^2)
  
  t1<-1+sqrt(m2/(m2-m1^2))
  t2<-(m2-sqrt(m2*(m2-m1^2)))/m1
  return(c(t1,t2))
}

#Fonction donnant G en fonction de theta_1

#Input:
#   theta_1 (float): theta_1
#Output:
#   float: valeur de G
G<-function(theta_1){
  return(1/(2*theta_1-1))
}

#Exemples
t1<-3
t2<-1
n<-20
smpl<-rdist(n,t1,t2)
h<-hist(smpl,breaks=50,freq = FALSE)
xh<-h$mids
yh<-c()
for (i in 1:length(xh)){
  yh[i]<-ddist(xh[i],t1,t2)
}

lines(xh,yh)
abline(v=t2)

tMLE<-theta_MLE(smpl)
tMME<-theta_MME(smpl)

tMLE
tMME

G_est<-c(G(tMLE[1]),G(tMME[1]))
G<-G(t1)

G_est
G

