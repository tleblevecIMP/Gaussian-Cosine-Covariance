# this code aims at verify that the corresponding covariance is properly simulated

monte_carlo_cov(range,freq,N,Ntest){
  mean_cov<-numeric(300)
  dist <- (1:300)/1000
  for ( i in 1:Ntest){
    t<-cov_1d(line_simulation(range,freq,1,1,N,1000),300,1000)
    mean_cov<- mean_cov + t 
  }
  mean_cov <- mean_cov/ Ntest
  plot(dist,mean_cov,type="l")
  curve(exp(-(x/range)^2)*cos(freq*x),add=TRUE)
}