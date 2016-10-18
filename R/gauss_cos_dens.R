# density associated with the fourier transform of the gaussian cosine covariance

gauss_cos_dens<-function(x,range=1,freq=10){

  return ((range/(4*sqrt(pi)))*(exp(-(1/4)*(range^2)*((freq-x)^2))+exp(-(1/4)*(range^2)*((freq+x)^2))))
}