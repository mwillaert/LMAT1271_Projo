#Generating the sample

source("toolbox.R")

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

