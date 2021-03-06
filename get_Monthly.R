# Ivan Lin, Zeyi Guo, Jingwen Chen
# this R scirpt is written to get monthly log-return stocks.
library(quantmod)
get_Monthly<-function(symbol, startDate, endDate){
  #make date in consistent format
  startdate<-format(as.Date(startDate), format='%Y-%m-%d')
  enddate<-format(as.Date(endDate), format='%Y-%m-%d')
  tryCatch(
    {
      #get only open price and close price
      getSymbols(symbol,src="yahoo",)
      price<-na.approx(get(symbol)[paste(startdate, '/', enddate, sep = '')][,c(1,4)])
      names(price)[1]<-"open"
      names(price)[2]<-"close"
      monthly<-monthlyReturn(price,type='log')
      return(monthly$monthly.returns)
    },error = function(err) { # catch Error
      msg <- paste('Error: Failed to get historical data of', symbol)
      print(msg)
      return(msg)
    }
  )
}