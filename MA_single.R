#Ivan Lin, Zeyi Guo, Jingwen Chen
#this R script is written to apply simple moving average strategy to single stock
source("get_Stock_MA.R")
MA_single<-function(symbol,short,l,startdate,enddate)
{
  #L is the lag factor, for example, it can take value of 20,30...
  L<-as.integer(l)
  startDay<-format(as.Date(startdate), format='%Y-%m-%d')
  endDay<-format(as.Date(enddate), format='%Y-%m-%d')
  asset<-get_Stock_MA(symbol,startDay,endDay)
  asset$no_short<-NA
  asset$can_short<-NA
  for(t in c((L+1):(length(asset$close))))
  {
    #MA is the moving average price factor, it is the mean of stock price in last L days
    MA<-0
    for(a in c((t-L):(t-1)))
    {
      MA<-MA+as.numeric(asset[a,1])
    }
    MA<-MA/L
    #if MA is smaller than price at day t-1, construct long position at day t
    if(MA<as.numeric(asset[t-1,1])){
      asset$no_short[t]<-asset$Return[t]
      asset$can_short[t]<-asset$Return[t]
    }else{
      #if can't short, we don't build position at day t
      #if can short, we build short position
      asset$no_short[t]<-0
      asset$can_short[t]<--asset$Return[t]
    }
  }
  if(short==0){
    return(asset$no_short[-c(1:L)])
  }else{
    return(asset$can_short[-c(1:L)])
  }
}