#Question 1.h
source("toolbox.R")

#Taille des echantillons utilisés pour estimer G
smpl_sizes=c(12,40,60,80,100,150,200,300,400,500)
n_smpl_sizes=length(smpl_sizes)
#nombre d'estimation a chaque fois
N_est<-1e3

#matrix with biases, variances and quad. errors
#3 rows for bias, variance, quad. err
#1 column for every smpl size
MLE_quality_matrix=matrix(nrow=3,ncol=n_smpl_sizes)
MME_quality_matrix=matrix(nrow=3,ncol=n_smpl_sizes)

#Donnees
t1<-3
t2<-1
G<-G_exact(t1)

#big loop to compute the biases, variances, and quad. errors
for(i in 1:n_smpl_sizes){
  #generating estimations
  G_estimates<-repeat_estimate(N_est,smpl_sizes[i],t1,t2)
  
  #quality estimate
  MLE_quality_matrix[,i]<-estimate_quality(G_estimates[1,],G)
  MME_quality_matrix[,i]<-estimate_quality(G_estimates[2,],G)
}

#lines

#Biases
plot(smpl_sizes,MLE_quality_matrix[1,],type="o",main="MLE Biases",xlab="sizes",ylab="bias")
plot(smpl_sizes,MME_quality_matrix[1,],type="o",main="MME Biases",xlab="sizes",ylab="bias")

#Variances
plot(smpl_sizes,MLE_quality_matrix[2,],type="o",main="MLE Variances",xlab="sizes",ylab="var")
plot(smpl_sizes,MME_quality_matrix[2,],type="o",main="MME Variances",xlab="sizes",ylab="var")

#Quad Errors
plot(smpl_sizes,MLE_quality_matrix[3,],type="o",main="MLE Quad. Errors",xlab="sizes",ylab="quad. err.")
plot(smpl_sizes,MME_quality_matrix[3,],type="o",main="MME Quad. Errors",xlab="sizes",ylab="quad. err.")