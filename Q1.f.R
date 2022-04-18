#Question 1.f

#Importe les fonctions de la question e
source("toolbox.R")

#Donnees
nsmpls<-1e3
smpl_size<-20
t1<-3
t2<-1
G<-G_exact(t1)

#On genere les estimations
G_estimates<-repeat_estimate(nsmpls,smpl_size,t1,t2)

#couleurs
c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")

#histogrammes
hMLE<-hist(G_estimates[1,],breaks=50,freq=FALSE)
hMME<-hist(G_estimates[2,],breaks=50,freq=FALSE)

#affichage
plot(hMLE,col=c1,xlim=c(0,0.4),ylim=c(0,100),xlab="estimations of G")
plot(hMME,col=c2,add=TRUE)

#indicateurs
abline(v=G,col="green")

#legende
legend(x="topright",legend=c("MLE","MME","Exact G"),fill=c(c1,c2,"green"))

dataG<-data.frame(MLE=G_estimates[1,],MME=G_estimates[2,])
boxplot(dataG)
abline(h=G,col="green")
legend(x="topright",legend="Exact G",fill="green")



