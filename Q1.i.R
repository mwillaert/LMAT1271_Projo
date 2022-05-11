#Question 1.i

source("toolbox.R")

#Number of estimators to be calculated for each histogram  
N <- 1e3

#Sample sizes to be used 
smpl_sizes <- c(2, 5, 10) 
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

#print(bias_sqr$one)
#print(bias_sqr$two)
#print(bias_sqr$three)

hist(bias_sqr$one)
hist(bias_sqr$two)
hist(bias_sqr$three)