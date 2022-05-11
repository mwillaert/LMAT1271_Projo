#Generating the sample

source("toolbox.R")
#Setting the parameters values
t1<-3
t2<-1

#Setting the number of samples
n<-20

#Generating a sample vector from the rdist function in toolbox 
smpl<-rdist(n,t1,t2)

#Defining a histogram using data from the sample vector
h<-hist(smpl, breaks = 5, freq = FALSE)
xh<-h$mids
yh<-c()
for (i in 1:length(xh)){
  yh[i]<-ddist(xh[i],t1,t2)
}

lines(xh,yh)
abline(v=t2)

tMLE<-theta_MLE(smpl)
tMME<-theta_MME(smpl)

#print(tMLE)
#print(tMME)

G_est<-c(G_exact(tMLE[1]),G_exact(tMME[1]))
G<-G_exact(t1)



print(G_est)
print(G)

