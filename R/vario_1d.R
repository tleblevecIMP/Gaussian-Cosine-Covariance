# compute a variogram on a 1d data set

vario_1d<-function(data,size,ncell){
  vario<-numeric(size)
  for (l in 1:size){
    npair = length(data)-l
    for ( pair in 1:npair){
      vario[l] = vario[l]+ (data[pair]-data[pair+l])^2
    }
    vario[l]=vario[l]/(2*npair)
  }
  dist = (1:size)/ncell
  plot(dist,vario,type="l")
  return(vario)
}