#Question 1.g

#Importe les fonctions de la question e
source("toolbox.R")

#Version Maxime (avec question g)

#Donnees
nsmpls<-1e3
smpl_size<-20
t1<-3
t2<-1
G<-G_exact(t1)

#On genere les estimations
G_estimates<-repeat_estimate(nsmpls,smpl_size,t1,t2)

#qualite des estimates
MLE_Q<-estimate_quality(G_estimates[1,],G)
MME_Q<-estimate_quality(G_estimates[2,],G)

MLE_mean<-MLE_Q[1]+G
MME_mean<-MME_Q[1]+G

MLE_stdev<-sqrt(MLE_Q[2])
MME_stdev<-sqrt(MME_Q[2])

MLE_err<-sqrt(MLE_Q[3])
MME_err<-sqrt(MME_Q[3])

#couleurs
c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")

#creation histogrammes
hMLE<-hist(G_estimates[1,],breaks=50,freq=FALSE)
hMME<-hist(G_estimates[2,],breaks=50,freq=FALSE)

#affichage MLE
plot(hMLE,col=c1,xlab="MLE estimations of G")

#indicateurs
abline(v=G,col="green")

abline(v=MLE_mean,col="red")

abline(v=MLE_mean-MLE_stdev,col="blue")
abline(v=MLE_mean+MLE_stdev,col="blue")

abline(v=G+MLE_err,col="orange")
abline(v=G-MLE_err,col="orange")

#legende
legend(x="topright",legend=c("Exact G","Mean Estimates","Std Dev","Quad Err"),fill=c("green","red","blue","orange"))

#affichage MME

plot(hMME,col=c2,xlab="MME estimations of G")

#indicateurs
abline(v=G,col="green")

abline(v=MME_mean,col="red")

abline(v=MME_mean-MME_stdev,col="blue")
abline(v=MME_mean+MME_stdev,col="blue")

abline(v=G+MME_err,col="orange")
abline(v=G-MME_err,col="orange")

#legende
legend(x="topright",legend=c("Exact G","Mean Estimates","Std Dev","Quad Err"),fill=c("green","red","blue","orange"))

#print de qualite
MLE_Q
MME_Q