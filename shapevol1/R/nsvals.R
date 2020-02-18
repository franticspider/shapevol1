

getSpan <- function(v){
  
  r <- range(v)
  
  return(r[2]-r[1])
  
}


getNSvals <- function(ps){
  
  xmean <- mean(ps$positions$X)
  ymean <- mean(ps$positions$Y)
  zmean <- mean(ps$positions$Z)
  
  xrange <- getSpan(ps$positions$X)
  yrange <- getSpan(ps$positions$Y)
  zrange <- getSpan(ps$positions$Z)
  
  nits <- ps$iterations
  npos <- nrow(ps$positions)
  
  return(data.frame(xmean,ymean,zmean,xrange,yrange,zrange,nits,npos ))
}