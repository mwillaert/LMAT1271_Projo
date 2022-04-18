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
G_exact<-function(theta_1){
  return(1/(2*theta_1-1))
}

#Pour question f

#Fonction generant N estimations de MLE et MME de G
#Input:
#   nsmpls (int): le nombre d'echantillons
#   smpl_size (int): la taille de chaque echatillon
#   theta_1,2 (float): les parametres de la distribution
#Output:
#   matrix of floats: premiere row contient les estimations MLE de G
#   la seconde contient les estimations MME

repeat_estimate<-function(nsmpls,smpl_size,theta_1,theta_2){
  #Generer les echantillon
  #chaque row est un echantillon
  smpls <- matrix(rdist(smpl_size*nsmpls,theta_1,theta_2),nrow=nsmpls)
  
  #estimations de G
  #premiere ligne: MLE, seconde ligne: MME
  G_estimates=matrix(nrow=2,ncol=nsmpls)
  for(i in 1:nsmpls){
    t1MLE<-theta_MLE(smpls[i,])[1]
    t1MME<-theta_MME(smpls[i,])[1]
    G_estimates[1,i]=G_exact(t1MLE)
    G_estimates[2,i]=G_exact(t1MME)
  }
  
  return(G_estimates)
}

#Fonction estimant la qualite d'un estimateur en calculant
#le biais, la variance et l'erreur quadratique moyenne

#Input:
#   -estimates (vector of floats): vecteur contenant estimations du parametre
#   -exact_value (float): valeur exact du parametre
estimate_quality<-function(estimates,exact_value){
  mean_estimate<-mean(estimates)
  
  bias<-mean_estimate-exact_value
  variance<-mean(estimates^2)-mean_estimate^2
  mean_quadratic_error<-bias^2+variance
  
  return(c(bias,variance,mean_quadratic_error))
}