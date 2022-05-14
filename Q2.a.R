#Importe fonctions
source("toolbox.R")

#Lecture du fichier
Data<-read.delim("Data.txt",sep=";")
x<-seq(5,47,length.out=100)

plot(Data$X,Data$Y)
plot(Data$X^(-1),Data$Y)
plot(Data$X^(-2),Data$Y)
plot(Data$X^(-3),Data$Y)
plot(Data$X^(-4),Data$Y)
#relation semble "la plus linéaire" pour Y*=Y et X=X^-2

par<-lin_reg_parameters(Data$X^(-2),Data$Y)
par2<-lin_reg_parameters(1/Data$X,Data$Y)
par3<-lin_reg_parameters(Data$X^(-3),Data$Y)

plot(Data$X^(-2),Data$Y)
lines(Data$X^(-2),Data$X^(-2)*par[2]+par[1])

plot(Data$X^(-1),Data$Y)
lines(Data$X^(-1),Data$X^(-1)*par2[2]+par2[1])

par
par2
par3

#test
n<-1000
s<-1
u<-seq(0,100,length.out=n)
y=rnorm(n,sd=s)
plot(u,y)
part<-lin_reg_parameters(u,y)
part