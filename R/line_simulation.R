# simulation in one dimension

line_simulation<-function(range,freq,length,seed,N,ncell){
  phase <- runif(N,0,2*pi)
  bin <- sample(c(-1,1),N,replace=T)
  w <- rnorm(N, mean = freq , sd = sqrt(2)/range)
  dist<-(1:ncell)/ncell
  sim<-numeric(ncell)
  for(i in 1:N){
    sim = sim + cos(bin[i]*w[i]*dist+phase[i])
  }
  sim = sim *sqrt(2/N)
  plot(dist,sim,type="l")
  
  return(sim)
}