# monte carlo simulation of the density associated as the gaussian cosine covariance
# it is a mixture of gaussian funtions symmetric

densi_simu<-function(N,range,freq){
  bin <- sample(c(-1,1),N,replace=T)
  w <- rnorm(N, mean = freq , sd = sqrt(2)/range)
  monte<-bin*w
  hist(monte)
  return(monte)
}