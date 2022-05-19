#Question 1.i

source("toolbox.R")

#Number of estimators to be calculated for each histogram  
N <- 1e3

#Sample sizes to be used 
smpl_sizes <- c(20, 100, 500) 
n_smpl_sizes <- length(smpl_sizes)

#List containing the vectors of bias * times sqrt(n) for each n
one <- c()
two <- c()
three <- c()
bias_sqr <- list(one = one, two = two, three = three)

#Generation of all samples and their corresponding estimators in a long loop
for(i in 1:n_smpl_sizes){ 
  #For each sample size n, a set of N MLEs is calculated in the first row 
  #of the following function
  G_estimates<-repeat_estimate(N,smpl_sizes[i], theta_1 = 3, theta_2 = 1)
  
  #Calculation of each vector and stocking them for each index of the list
  bias_sqr[[i]] <- sqrt(smpl_sizes[i])*(G_estimates[1,] - G_exact(theta_1 = 3))
}

#Essai avec info de Fisher
t1<-3
t2<-1
F<-1/9

I<-(2*t1-1)^3*( (2*t1-1)/(4*t1^2) - log(t2) - 1/t1 + t1*t2^t1 *F )
print(I)

h1<-hist(bias_sqr$one,breaks=16, freq=FALSE,main="Histogramme pour n=20")
x1<-h1$mids
lines(x1,dnorm(x1,sd=1/sqrt(I)),col="blue",lwd=2)
legend("topright",col="blue",legend="Distribution Limite",lty=1)

h2<-hist(bias_sqr$two,breaks=16,freq=FALSE,main="Histogramme pour n=100")
x2<-h2$mids
lines(x2,dnorm(x2,sd=1/sqrt(I)),col="blue",lwd=2)
legend("topright",col="blue",legend="Distribution Limite",lty=1)

h3<-hist(bias_sqr$three,breaks=16,freq=FALSE,main="Histogramme pour n=500")
x3<-h3$mids
lines(x3,dnorm(x3,sd=1/sqrt(I)),col="blue",lwd=2)
legend("topright",col="blue",legend="Distribution\nLimite",lty=1)

