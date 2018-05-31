# Ivan Lin, Zeyi Guo, Jingwen Chen
# this R script is written to develop traditional momentum strategy in US utility sector stocks(price greater than 5, and 93 stocks in total)
#the time span is input,for example from 2012-11-14 to 2017-11-14
source("get_Monthly.R")
momentum<-function(j,k,short,start,end)
{
  #j is observing period, k is holding period
  #read stock list from txt file and put them in a list
  ob<-as.integer(j)
  ho<-as.integer(k)
  startDay<-format(as.Date(start), format='%Y-%m-%d')
  endDay<-format(as.Date(end), format='%Y-%m-%d')
  data<-read.table("utility_list.txt",as.is=TRUE)
  l<-vector(mode="character",length=length(data))
  for(i in 1:length(data))
  {
    l[i]<-data[i]
  }
  #create a frame storing log-return of 94 stocks
  i<-l[1]
  pool<-get_Monthly(as.character(i),startDay,endDay)
  names(pool)[1]<-i
  j<-2
  for(i in l[2:length(l)])
  {
    pool$i<-get_Monthly(as.character(i),startDay,endDay)
    names(pool)[j]<-i
    j<-j+1
  }
  #delete the row if the valid number of monthly return in that row is less than half 
  r<-1
  while(r<=length(pool[,1]))
  {
    test<-0
    for(element in(is.na(pool[r,])))
    {
      if(element==F){
        test<-test+1
      }
    }
    if(test<length(data)/2){
      pool<-pool[-r,]
      r<-r-1
    }
    r<-r+1
  }
  pool$max_index<-NA
  pool$min_index<-NA
  pool$win<-NA
  pool$lose<-NA
  pool$both<-NA
  pool$market<-NA
  #compute market return as a benchmark
  for(market_row in c(1:length(pool$max_index)))
  {
    market<-0
    market_count<-0
    for(market_column in c(1:length(data)))
    {
      if(!is.na(pool[market_row,market_column])){
        market<-market+(exp(pool[market_row,market_column])-1)
        market_count<-market_count+1
      }
    }
    pool$market[market_row]<-(market/market_count)
  }
  #compute momentum strategy return
  count<-1
  for(t in c((ob+1):(ob+ho)))
  {
    li<-c()
    for(col in c(1:length(data)))
    {
      #calculate cumulative return for each column
      cum<-0
      for(ro in c((t-ob):(t-1)))
      {
        cum<-cum+as.numeric(pool[ro,col])
      }
      li<-c(li,cum)
    }
    #use mean of li to replace the NA, which will not affect max and min
    li[is.na(li)]<-mean(li,na.rm=T)
    #find which stocks have the highest/lowest cumulative return
    max_index<-which(li==max(li),arr.ind=TRUE)
    min_index<-which(li==min(li),arr.ind=TRUE)
    pool$max_index[t]<-max_index
    pool$min_index[t]<-min_index
    count_return_win<-0
    count_return_lose<-0
    for(b in c(0:(count-1)))
    {
      #get the max_index and min_index obtained from previous month up to time t
      x<-pool$max_index[t-b]
      y<-pool$min_index[t-b]
      count_return_win<-count_return_win+(exp(pool[t,as.integer(x)])-1)
      count_return_lose<-count_return_lose-(exp(pool[t,as.integer(y)])-1)
    }
    #calculate monthly returns based on winner and loser and both
    count_return_win<-count_return_win/count
    count_return_lose<-count_return_lose/count
    pool$win[t]<-count_return_win
    pool$lose[t]<-count_return_lose
    pool$both[t]<-count_return_win+count_return_lose
    count<-count+1
  }
  #for the next part, we continue the journey, but this time we don't need count because it is a steady state
  for(t in c((ob+ho+1):length(pool$max_index)))
  {
    li<-c()
    for(col in c(1:length(data)))
    {
      #calculate cumulative return for each column
      cum<-1
      for(ro in c((t-ob):(t-1)))
      {
        cum<-cum*(1+as.numeric(pool[ro,col]))
      }
      cum<-cum-1
      li<-c(li,cum)
    }
    #use mean of li to replace the NA, which will not affect max and min
    li[is.na(li)]<-mean(li,na.rm=T)
    #find which stocks have the highest/lowest cumulative return
    max_index<-which(li==max(li),arr.ind=TRUE)
    min_index<-which(li==min(li),arr.ind=TRUE)
    pool$max_index[t]<-max_index
    pool$min_index[t]<-min_index
    count_return_win<-0
    count_return_lose<-0
    for(b in c(0:(count-1)))
    {
      #get the max_index and min_index obtained from previous month up to time t
      x<-pool$max_index[t-b]
      y<-pool$min_index[t-b]
      count_return_win<-count_return_win+(exp(pool[t,as.integer(x)])-1)
      count_return_lose<-count_return_lose-(exp(pool[t,as.integer(y)])-1)
    }
    #calculate monthly returns based on winner and loser and both
    count_return_win<-count_return_win/ho
    count_return_lose<-count_return_lose/ho
    pool$win[t]<-count_return_win
    pool$lose[t]<-count_return_lose
    pool$both[t]<-count_return_win+count_return_lose
  }
  #if we can't short sale
  if(short==0){
    return(pool[-c(1:ob),c((length(data)+3),(length(data)+6))])
  }else {
    #if we can short sale
    return(pool[-c(1:ob),c((length(data)+5),(length(data)+6))])
  }
}

