qdist<-function(t,theta_1,theta_2){
  result <- theta_2 * (1-t)^(-1/theta_1)
  return(result)
}

rdist<-function(smpl_size,theta_1,theta_2){
  
}