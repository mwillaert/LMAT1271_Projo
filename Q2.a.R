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