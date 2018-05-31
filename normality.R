library(nortest)
normality<-function(num){
  l<-length(num)
  nx<-vector(mode="numeric",length=l)
  for(i in c(1:l)){
    nx[i]<-as.numeric(num[i][1])
  }
  p_value<-round(lillie.test(nx)$p.value,10)
  if(p_value>0.05){
    result<-list(p_value, "Data conforms to normal distribution")
    return(result)
  }else{
    result<-list(p_value, "Data doesn't conform to normal distribution")
    return(result)
  }
}
