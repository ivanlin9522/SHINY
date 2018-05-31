# Ivan Lin, Zeyi Guo, Jingwen Chen
# this R script is written to fulfill the histogram plotting requirement
plot_hist<-function(symbol,logReturn)
{
  #plot histogram of logReturn given the symbol ticker and add a normal probability plot
  lower_bound=min(logReturn)
  upper_bound=max(logReturn)
  return({
    par(bg=rgb(199,180,167,maxColorValue=255))
    hist(logReturn,
         freq=FALSE,
         density=NULL,
         main=paste("Log-return Histogram of",toString(symbol),sep = " "),
         xlim=c(lower_bound,upper_bound),
         col=c(rgb(255,127,36,maxColorValue=255),rgb(238,118,33,maxColorValue=255),rgb(205,102,29,maxColorValue=255),rgb(139,69,19,maxColorValue=255))
         )
    plot(function(x) dnorm(x,mean(logReturn), sqrt(var(logReturn))), 
         xlim=c(lower_bound, upper_bound),
         add=TRUE, 
         col=rgb(87, 96, 105, maxColorValue = 255),
         lwd=3)
  })
}

#next we create confidence intervals for the means and variances given a confidence level
confidence_intervals<-function(symbol,logReturn,alpha)
{
  #initialize summary
  confidence_intervals_summary <- NULL
  n<-length(logReturn)
  mean<-mean(logReturn)
  var<-var(logReturn)
  #Given unknown variance, we should use t-distribution to estimate mean intervals
  mean_fluctuation<-qt(1-(alpha/2), df=(n-1))*sqrt(var)/sqrt(n)
  mean_lower_lim=mean-mean_fluctuation
  mean_upper_lim=mean+mean_fluctuation
  confidence_intervals_summary$mean_lower<-round(mean_lower_lim,10)
  confidence_intervals_summary$mean_upper<-round(mean_upper_lim,10)
  #Given unknown mean, we should use chi_square-distribution to estimate variance intervals
  var_lower_lim<-(n-1)*var/qchisq(1-(alpha/2),df=(n-1))
  var_upper_lim<-(n-1)*var/qchisq(alpha/2,df=(n-1))
  confidence_intervals_summary$var_lower<-round(var_lower_lim,10)
  confidence_intervals_summary$var_upper<-round(var_upper_lim,10)
  return(confidence_intervals_summary)
}

#next perform a regression of the log-return on time
regression_one<-function(logReturn)
{
  t=time(logReturn)
  lm_model<-lm(logReturn~t,data=data.frame(t,logReturn))
  return(lm_model)
}

#next test equality of the two population mean
equal_test<-function(logReturn1,logReturn2,alpha)
{
  equal_test_summary<-NULL
  mean1<-mean(logReturn1)
  mean2<-mean(logReturn2)
  var1<-var(logReturn1)
  var2<-var(logReturn2)
  n1<-length(logReturn1)
  n2<-length(logReturn2)
  #first check if two variances are the same
  F_statistics<-var1/var2
  F_lower<-qf(alpha/2,n1-1,n2-1)
  F_upper<-qf(1-alpha/2,n1-1,n2-2)
  if(F_statistics<F_lower||F_statistics>F_upper)
  {
    #two variances are equal, so we can use t-statistics
    Sw<-sqrt(((n1-1)*var1+(n2-1)*var2)/(n1+n2-2))
    t_statistics<-(mean1-mean2)/(Sw*sqrt(1/n1+1/n2))
    pvalue<-2*pt(-abs(t_statistics),df=(n1+n2-2))
    equal_test_summary$pvalue<-round(pvalue,5)
    if(pvalue<alpha)
    {
      equal_test_summary$conclusion<-"Reject null hypothesis"
    }
    else
    {
      equal_test_summary$conclusion<-"Accept null hypothesis"
    }
  }
  else
  {
    #two variances are unequal,so we can use z-statistics assuming big sample
    z_statistics<-(mean1-mean2)/(sqrt(var1/n1+var2/n2))
    pvalue<-2*pnorm(-abs(z_statistics))
    equal_test_summary$pvalue<-pvalue
    if(pvalue<alpha)
    {
      equal_test_summary$conclusion<-"Reject null hypothesis"
    }
    else
    {
      equal_test_summary$conclusion<-"Accept null hypothesis"
    }
  }
  return(equal_test_summary)
}

#next perform a regression of one log-return on the other
regression_two<-function(logReturn1,logReturn2)
{
  lm_model<-lm(logReturn2~logReturn1,data=data.frame(logReturn1, logReturn2))
  return(lm_model)
}





