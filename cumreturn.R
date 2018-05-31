cumreturn<-function(ret){
  l<-length(ret)
  cum<-1
  cumret<-vector(mode="numeric",length=l)
  for(i in c(1:l)){
    cumret[i]<-(1+as.numeric(ret[i][1]))*cum-1
    cum<-cum*(1+as.numeric(ret[i][1]))
  }
  return(cumret)

}
