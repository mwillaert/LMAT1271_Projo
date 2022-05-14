#Fonction calculant le quantile Q(t) pour la distribution de l'?nonc?
#Input:
#   t (float): variable de la fonction quantile Q(t)
#   theta_1,2 (float): param?tres de la distribution
#Output:
#   float: quantile Q(t)

qdist<-function(t,theta_1,theta_2){
  result <- theta_2 * (1-t)^(-1/theta_1)
  return(result)
}

#Fonction permettant de g?n?rer un echantillon iid de taille n suivant
#la distribution de l'?nonc?
#Input:
#   smpl_size (int): taille de l'echantillon
#   theta_1,2 (float): param?tres de la distribution
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

#Fonction donnant la densit? de probabilit? de l'?nonc?
#Input:
#   x (float): variable de la densit? f(x)
#   theta_1,2 (float): param?tres de la distribution
#Ouput:
#   float: densit? au point x
ddist<-function(x,theta_1,theta_2){
  result<-theta_1 * (theta_2^theta_1)/(x^(theta_1+1))
  return(result)
}

#Estimateurs

#Estimateurs MLE de theta_1,2
#Input:
#   smpl (vector of floats): echantillon g?n?r? par theta
#Output:
#   vector of floats: estimateurs MLE (t_1,t_2) de theta_1,theta_2

theta_MLE<-function(smpl){
  t2<-min(smpl)
  #moyenne geometrique de l'echantillon
  gm<-exp(mean(log(smpl)))
  t1<- 1/log(gm/t2)
  return(c(t1,t2))
}


#Estimateurs MME de theta_1,2
#Input:
#   smpl (vector of floats): echantillon g?n?r? selon la dist de l'?nonc?
#Output:
#   vector of floats: estimateurs MME (t_1,t_2) de theta_1,theta_2

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
#   nsmpls (int): le nombre d'echantillons N
#   smpl_size (int): la taille de chaque echantillon n
#   theta_1,2 (float): les param?tres de la distribution
#Output:
#   matrix of floats: de dim 2 x nsmpls, la premiere 
#   row contient les estimations MLE de G
#   la seconde contient les estimations MME de G

repeat_estimate<-function(nsmpls,smpl_size,theta_1,theta_2){
  #Generer les echantillon
  #Chaque row est un echantillon de taille nsmpls
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

#Fonction estimant la qualit? d'un estimateur en calculant
#le biais, la variance et l'erreur quadratique moyenne

#Input:
#   -estimates (vector of floats): vecteur contenant estimations du parametre
#   -exact_value (float): valeur exact du parametre
#Output:
#   -bias, variance, mean quadratic error (vector of floats)
estimate_quality<-function(estimates,exact_value){
  mean_estimate<-mean(estimates)
  
  bias<-mean_estimate-exact_value
  variance<-mean(estimates^2)-mean_estimate^2
  mean_quadratic_error<-bias^2+variance
  
  return(c(bias,variance,mean_quadratic_error))
}


#######################################################################
#Linear regression Part

#Fonction calculant "produit scalaire" de deux échantillons (en retirant moyenne)
#Sum_i (xi-x)(yi-y)

#Input:
#   -x,y (vector of floats): échantillons
#Output:
#   -float: produit
S<-function(x,y){
  n<-length(x)
  est<-0
  mx<-mean(x)
  my<-mean(y)
  
  for(i in 1:n){
    est<-est + (x[i]-mx)*(y[i]-my)
  }
  return(est)
}

#Fonction donnant les estimateurs des parametres pour une regression lineaire simple
#(methode des moindres carres) ainsi que le facteur de Correlation R2 et la p-value pour
#l'hypothèse b1=0
#Y=b0+b1*X
#
#Input:
#   -x,y (vector of floats): échantillons
#Output:
#   -vector of floats:bO,b1,R2,p
lin_reg_parameters<-function(x,y){
  n<-length(x)
  Sxy<-S(x,y)
  Sxx<-S(x,x)
  Syy<-S(y,y)
  
  #line parameters
  b1<-Sxy/Sxx
  b0<-mean(y)-b1*mean(x)
  
  #correlation
  R2<-Sxy^2/(Sxx*Syy)
  
  #estimator of sigma
  s<-sqrt((Syy-b1*Sxy)/(n-2))
  
  #p-value
  Tobs<-(sqrt(Sxx)*b1)/s
  p<-2*pt(-abs(Tobs),df=n-2)
  return(c(b0,b1,R2,p))
}

