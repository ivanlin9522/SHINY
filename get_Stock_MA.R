# Ivan Lin, Zeyi Guo, Jingwen Chen
# this R scirpt is written to get price for stocks for MA edition.
library(quantmod)
get_Stock_MA<-function(symbol, startDate, endDate){
  #make date in consistent format
  startdate<-format(as.Date(startDate), format='%Y-%m-%d')
  enddate<-format(as.Date(endDate), format='%Y-%m-%d')
  tryCatch(
    {
      #get only open price and close price
      getSymbols(symbol,src="google",)
      price<-get(symbol)[paste(startdate, '/', enddate, sep = '')][,c(1,4)]
      names(price)[1]<-"open"
      names(price)[2]<-"close"
      #delete row if its value is NA
      for(r in c(1:length(price[,1])))
      {
        if(is.na(price[r,1]))
        {
          price<-price[-r,]
        }
      }
      #now compute logReturn
      for (i in 1:length(price$open))
      {
        price$Return[i]<-(price$close[i]/price$open[i])-1
      }
      #return column close and return, that's what we need in MA strategy
      return(price[,-1])
    },error = function(err) { # catch Error
      msg <- paste('Error: Failed to get historical data of', symbol)
      print(msg)
      return(msg)
    }
  )
}