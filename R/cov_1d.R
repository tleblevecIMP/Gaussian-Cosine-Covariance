# compute the covariance on a 1d data set

cov_1d<-function(data,size,ncell){
  cov<-numeric(size)
  for (l in 1:size){
    npair = length(data)-l
    for ( pair in 1:npair){
      cov[l] = cov[l]+ data[pair]*data[pair+l]
    }
    cov[l]=cov[l]/(npair)
  }
  cov = (cov - mean(data)^2 )/ var(data) # we are measuring a correlation
  dist = (1:size)/ncell
  return(cov)
}