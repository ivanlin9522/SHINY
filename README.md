# SHINY
Statistics Analysis and Trending Strategies using Shiny App
The file includes explanation for each part of the shiny app.

Please note: because this app contains complex function and needs heavy computation, it can take several minutes to fully open the website and load all the graphs and data. 

app.R is our main R file.

cumreturn.R calculate the cumulative return that will be used in simple moving average strategy.

get_Monthly.R calculate monthly return that will be used in momentum strategy.

get_StockPrice.R use quantmod package to get stock price. It can show the logreturn of every symbol you input.

MA_single.R perform simple moving average strategy.

momentum.R perform momentum strategy.

normality.R test the normality of the logreturn.

plot_data.R contains plotting functions that will show histogram and linear regression graph.

utility_list.txt contains 93 stock symbols in US utility sector.

If you find the trending strategy part confusing, you can refer to my undergraduate thesis "Empirical Study on trending strategies in Chinese commodity futures market".
