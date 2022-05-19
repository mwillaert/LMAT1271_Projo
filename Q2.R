#Importe fonctions
source("toolbox.R")

#Lecture du fichier
Data<-read.delim("Data.txt",sep=";")
x<-seq(5,47,length.out=100)
ordered_X<-sort(Data$X)

#Affichage de différents couples X*, Y*
plot(Data$X,Data$Y,xlab="Electricity Consumption (MWh)",ylab="Productivity (1000€/day)")
plot(Data$X^(-1),Data$Y)
plot(Data$X^(-2),Data$Y)
plot(Data$X^(-3),Data$Y)
plot(Data$X^(-4),Data$Y)
#relation semble "la plus linéaire" pour Y*=Y et X=X^-2

#Calcul des paramètres de régression linéaire
par<-lin_reg_parameters(Data$X^(-1),Data$Y)
par2<-lin_reg_parameters(Data$X^(-2),Data$Y)
par3<-lin_reg_parameters(Data$X^(-3),Data$Y)

plot(Data$X^(-1),Data$Y)
lines(Data$X^(-1),Data$X^(-1)*par[2]+par[1])

plot(Data$X^(-2),Data$Y)
lines(Data$X^(-2),Data$X^(-2)*par2[2]+par2[1])

plot(Data$X,Data$Y,xlab="Electricity Consumption (MWh)",ylab="Productivity (1000€/day)")
lines(ordered_X,par2[1]+par2[2]*ordered_X^(-2),col="blue",lwd=2)
legend(22,8,legend="tendance non-linéaire",col="blue",lty=1)

plot(Data$X,Data$Y,xlab="Electricity Consumption (MWh)",ylab="Productivity (1000€/day)")
lines(ordered_X,par[1]+par[2]*ordered_X^(-1),col="blue",lwd=2)
legend(22,8,legend="tendance non-linéaire",col="blue",lty=1)

#Affichage des résultats
print(par)
print(par2)
print(par3)

#Résultats (b0, b1, R2, p-value) avec Y*=b0+b1*X*
#Pour X*=1/X et Y*=Y:  2.717689e+01 -6.295179e+01  4.841859e-01  6.210726e-07
#Pour X*=1/X^2 et Y*=Y: 2.300021e+01 -2.038217e+02  4.670521e-01  1.174551e-06

