# Ivan Lin, Zeyi Guo, Jingwen Chen
# this R scirpt is written to get price for stocks.
library(quantmod)
get_StockPrice<-function(symbol, startDate, endDate){
  #make date in consistent format
  startdate<-format(as.Date(startDate), format='%Y-%m-%d')
  enddate<-format(as.Date(endDate), format='%Y-%m-%d')
  tryCatch(
    {
      #get only open price and close price
      getSymbols(symbol,src="google",)
      price<-na.approx(get(symbol)[paste(startdate, '/', enddate, sep = '')][,c(1,4)])
      names(price)[1]<-"open"
      names(price)[2]<-"close"
      #now compute logReturn
      for (i in 1:length(price$open))
      {
        price$logReturn[i] = log(price$close[i]/price$open[i])
      }
      return(price$logReturn)
    },error = function(err) { # catch Error
      msg <- paste('Error: Failed to get historical data of', symbol)
      print(msg)
      return(msg)
    }
  )
}

  
  
  
  
  
  
  
  
  
  
  
  
  
  
