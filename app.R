#Ivan Lin, Zeyi Guo, Jingwen Chen
#make sure required library has been installed, namely shiny, quantmod
#Can't have two same outputId or inputID!!! Lesson I learned bloodily.
if (!require(shiny))
{
  install.packages('shiny')
}
if (!require(quantmod))
{
  install.packages('quantmod')
}
if (!require(nortest))
{
  install.packages('nortest')
}

library(shiny)
library(nortest)
source("get_Monthly.R")
source("get_Stock_MA.R")
source("get_StockPrice.R")
source("MA_single.R")
source("momentum.R")
source("plot_data.R")
source("normality.R")
source("cumreturn.R")

ui<-fluidPage(
  withMathJax(),
  tags$link(rel = "stylesheet", type = "text/css", href = "mystyle.css"),
  tags$body(background="ball.gif"),
  fluidRow(
    column(7,tags$h1(tags$strong("Statistics Analysis"),"and",tags$strong("Trending Strategies"))),
    column(2,tags$img(width=90,height=70,src="button.png")),
    column(2,tags$br(), tags$h3(class="bottom", "Trinity Presents"))
  ),
  tabsetPanel(
    tabPanel("Statistics Analysis",
             tabsetPanel(
               tabPanel("One Symbol",
                        sidebarLayout(
                          sidebarPanel(
                            wellPanel(
                              tags$h4(class="center", "Please put in the ticker symbol that interests you."),
                              textInput(inputId='symbol_one', label='Symbol', value='AAPL')),
                            wellPanel(
                              tags$h4(class="center", "Choose time range for plotting histogram and performing regression."),
                              dateRangeInput(inputId="dates_one", 
                                             label="Date range",
                                             start = "2015-01-01", 
                                             end = as.character(Sys.Date()),
                                             max = as.character(Sys.Date()),
                                             min="2000-01-01"
                              )
                            ),
                            wellPanel(
                              tags$h4(class="center", "Change the value of alpha for mean confidence interval."),
                              sliderInput(inputId='alpha_in_one', 
                                          label='Significance Level', 
                                          min = 0.01,
                                          max = 0.4,
                                          step = 0.01,
                                          value = 0.05)
                            )),
                          mainPanel(
                            fluidRow(
                              column(6,
                                     tabsetPanel(
                                       tabPanel("Histogram",
                                                wellPanel(
                                                  tags$h4(class="center","Statistics of log-return of the stock"),
                                                  tags$table(class="center_table",
                                                             tags$tr(tags$th("Mean: "),tags$td(textOutput(outputId = "mean_one", inline = TRUE))),
                                                             tags$tr(tags$th("Variance: "),tags$td(textOutput(outputId = "variance_one"), inline = TRUE))),
                                                  plotOutput(outputId = "histogram_one"),
                                                  tags$h4(class="center", "Normality Test: Lilliefor Test"),
                                                  tags$h4(class="center", "p-value: ",tags$span(textOutput(outputId = "norm_pvalue",inline=TRUE))),
                                                  tags$h4(class="center",textOutput(outputId = "norm_result"))
                                                )
                                       ),
                                       tabPanel("Regression",
                                                wellPanel(
                                                  tags$table(
                                                    class = "center_table",
                                                    tags$tr(
                                                      tags$th("Intercept Estimate: "),
                                                      tags$td(textOutput(outputId = "regression_intercept_one", inline = TRUE))
                                                    ),
                                                    tags$tr(
                                                      tags$th("Slope Estimate: "),
                                                      tags$td(textOutput(outputId = "regression_slope_one", inline = TRUE))
                                                    ),
                                                    tags$tr(
                                                      tags$th("\\(\\mathbf R^2\\):"),
                                                      tags$td(textOutput(outputId = "regression_r_square_one", inline = TRUE))
                                                    )
                                                  )
                                                ),
                                                tabsetPanel(
                                                  tabPanel("Diagram of data with LS line",
                                                           wellPanel(
                                                             tags$h4(class="center","Regression of the log-return on time"),
                                                             plotOutput(outputId = "regression_LS_one")
                                                           )
                                                  ),
                                                  tabPanel("Residual Graph",
                                                           wellPanel(
                                                             tags$h4(class="center","Graphical depiction of residuals"),
                                                             plotOutput(outputId = "regression_residual_one")
                                                           )
                                                  )
                                                )
                                       )
                                     )
                              ),
                              column(6,
                                     tags$br(),tags$br(),
                                     wellPanel(
                                       tags$h4(class="center",
                                              "Formula to compute confidence interval for mean:"
                                              ),
                                       tags$h4(class="center",
                                              "$$\\mathbf {(\\bar X - \\frac{S_X}{\\sqrt{n}}t_\\frac{\\alpha}{2}, \\bar X + \\frac{S_X}{\\sqrt{n}}t_\\frac{\\alpha}{2})}$$"
                                       ),tags$br(),
                                       tags$h4(class="center",
                                                       tags$span("The "),
                                                       textOutput(outputId = "alpha_out_mean_one", inline=TRUE),
                                                       tags$span("% Confidence interval for the mean: ")
                                                      ),
                    
                                       tags$h4(class="center",
                                             tags$span("("),
                                             textOutput(outputId ="mean_lower_one", inline = TRUE),
                                             tags$span(","),
                                             textOutput(outputId ="mean_upper_one", inline = TRUE),
                                             tags$span(")")
                                            )
                                       ),
                                     tags$br(),tags$br(),tags$br(),tags$br(),
                                     wellPanel(
                                       tags$h4(class="center",
                                               "Formula to compute confidence interval for variance:"
                                       ),
                                       tags$h4(class="center",
                                               "$$\\mathbf {(\\frac{(n-1)S_X^2}{\\chi^2_\\frac{\\alpha}{2}}, \\frac{(n-1)S_X^2}{\\chi^2_\\frac{2-\\alpha}{2}})}$$"
                                       ),tags$br(),
                                     tags$h4(class="center",
                                             tags$span("The "),
                                             textOutput(outputId = "alpha_out_variance_one", inline=TRUE),
                                             tags$span("% Confidence interval for the variance: ")
                                     ),
            
                                     tags$h4(class="center",
                                             tags$span("("),
                                             textOutput(outputId ="variance_lower_one", inline = TRUE),
                                             tags$span(","),
                                             textOutput(outputId ="variance_upper_one", inline = TRUE),
                                             tags$span(")")
                                     )
                                     )
                              )
                            )
                          )
                        )
               ),
               tabPanel("Two Symbols",
                        sidebarLayout(
                          sidebarPanel(
                            wellPanel(
                              tags$h4(class="center", "Please put in two symbols that interest you."),
                              textInput(inputId='symbol_two_1', label='Symbol 1', value='AAPL'),
                              textInput(inputId='symbol_two_2', label='Symbol 2', value='AMZN')
                            ),
                            wellPanel(
                              tags$h4(class="center", "Choose time range to test the equality of the two population means and perform regression."),
                              dateRangeInput(inputId="dates_two", 
                                             label="Date range",
                                             start = "2015-01-01", 
                                             end = as.character(Sys.Date()),
                                             max = as.character(Sys.Date()),
                                             min="2000-01-01"
                              )
                            ),
                            wellPanel(
                              tags$h4(class="center", "Change the value of alpha for hypothesis testing."),
                              sliderInput(inputId='alpha_in_two', 
                                          label='Significance Level', 
                                          min = 0.01,
                                          max = 0.4,
                                          step = 0.01,
                                          value = 0.05)
                            )
                          ),
                          mainPanel(
                            fluidRow(
                              column(6,
                                     wellPanel(
                                       wellPanel(
                                         tags$h4(class="center", "Test the equality of the two population means"),
                                         tags$h5(class="center", "\\[\\mathbf {H_0: \\mu_X=\\mu_Y; H_1: \\mu_X\\ne\\mu_Y}\\]")
                                       ),
                                       wellPanel(
                                         tags$h4(class="center", "Use of Statistics: "),
                                         tags$h4(class="center", "With unequal variance but big sample, use Z"),
                                         tags$h4(class="center", "With equal variance assumption, use T"),
                                         tags$h5(class="center", "$$\\mathbf {Z=\\frac{(\\bar X - \\bar Y) - (\\mu_X - \\mu_Y)}{\\sqrt{\\frac{S_X^2}{n} + \\frac{S_Y^2}{m}}} {\\sim} N(0,1)}$$"),
                                         tags$h5(class="center", "$$\\mathbf {T=\\frac{(\\bar X - \\bar Y) - (\\mu_X - \\mu_Y)}{Sw\\sqrt{\\frac{1}{n} + \\frac{1}{m}}} {\\sim} t(n+m-2)}$$"),
                                         tags$h5(class="center", "$$\\mathbf {Sw=\\sqrt{\\frac{(n-1)S_X^2 + (m-1)S_Y^2}{n+m-2}}}$$")
                                       ),
                                       wellPanel(
                                         tags$h4(class="center", 
                                                 tags$span("p-value: "),
                                                 textOutput(outputId = "pvalue", inline = TRUE)
                                         ),
                                         tags$h4(class="center",
                                                 tags$span("Result of the test: "),
                                                 textOutput(outputId = "conclusion", inline = TRUE)
                                         )
                                       )
                                     )
                              ),
                              column(6,
                                     wellPanel(
                                       wellPanel(
                                         tags$table(
                                           class = "center_table",
                                           tags$tr(
                                             tags$th("Intercept Estimate: "),
                                             tags$td(textOutput(outputId = "regression_intercept_two", inline = TRUE))
                                           ),
                                           tags$tr(
                                             tags$th("Slope Estimate: "),
                                             tags$td(textOutput(outputId = "regression_slope_two", inline = TRUE))
                                           ),
                                           tags$tr(
                                             tags$th("\\(\\mathbf R^2\\):"),
                                             tags$td(textOutput(outputId = "regression_r_square_two", inline = TRUE))
                                           )
                                         )
                                       ),
                                       
                                       tabsetPanel(
                                         tabPanel("Diagram of data with LS line",
                                                  wellPanel(
                                                    tags$h4(class="center","Regression of one log-return on the other"),
                                                    plotOutput(outputId = "regression_LS_two")
                                                  )
                                         ),
                                         tabPanel("Residual Graph",
                                                  wellPanel(
                                                    tags$h4(class="center","Graphical depiction of residuals"),
                                                    plotOutput(outputId = "regression_residual_two")
                                                  )
                                         )
                                       )
                                     )
                              )
                            )
                          )
                        )
               )
             )
    ),
    tabPanel("Trending Strategies",
             sidebarLayout(
               sidebarPanel(
                 wellPanel(
                   tags$h4(class="center", "Choose the observing period J for momentum strategy."),
                   sliderInput(inputId='observe', 
                               label='Observing Period J', 
                               min = 1,
                               max = 12,
                               step = 1,
                               value = 3),
                   tags$h4(class="center", "Choose the holding period K for momentum strategy."),
                   sliderInput(inputId='hold', 
                               label='Holding Period K', 
                               min = 1,
                               max = 12,
                               step = 1,
                               value = 5),
                   tags$h4(class="center", "Choose time range for performing momentum strategy."),
                   dateRangeInput(inputId="dates_momentum", 
                                  label="Date range",
                                  start = "2014-01-01", 
                                  end = "2017-11-01",
                                  max = as.character(Sys.Date()),
                                  min="2000-01-01"
                   )
                 ),
                 wellPanel(
                   tags$h4(class="center", "Please put in the ticker symbol that interests you for moving average strategy."),
                   textInput(inputId='symbol_ma', label='Symbol', value='AAPL'),
                   tags$h4(class="center", "Choose the lagging length L for moving average strategy."),
                   sliderInput(inputId='lag', 
                               label='Lagging Length L', 
                               min = 10,
                               max = 50,
                               step = 10,
                               value = 20),
                   tags$h4(class="center", "Choose time range for performing moving average strategy."),
                   dateRangeInput(inputId="dates_ma", 
                                  label="Date range",
                                  start = "2015-01-01", 
                                  end = as.character(Sys.Date()),
                                  max = as.character(Sys.Date()),
                                  min="2000-01-01"
                   )
                 )
               ),
               mainPanel(
                 fluidRow(
                   column(6,
                          wellPanel(
                            tags$h3(class="center", "Traditional Momentum Strategy"),
                            tabsetPanel(
                              tabPanel("Regression without shorting",
                                       wellPanel(
                                         tags$table(
                                           class = "center_table",
                                           tags$tr(
                                             tags$th("Intercept Estimate: "),
                                             tags$td(textOutput(outputId = "regression_momentum_no_short_intercept", inline = TRUE))
                                           ),
                                           tags$tr(
                                             tags$th("Slope Estimate: "),
                                             tags$td(textOutput(outputId = "regression_momentum_no_short_slope", inline = TRUE))
                                           ),
                                           tags$tr(
                                             tags$th("\\(\\mathbf R^2\\):"),
                                             tags$td(textOutput(outputId = "regression_momentum_no_short_r_square", inline = TRUE))
                                           )
                                         )
                                       ),
                                       tabsetPanel(
                                         tabPanel("Diagram of data with LS line",
                                                  wellPanel(
                                                    tags$h4(class="center","Regression of the strategy return on market return"),
                                                    plotOutput(outputId = "regression_momentum_no_short_LS")
                                                  )
                                         ),
                                         tabPanel("Residual Graph",
                                                  wellPanel(
                                                    tags$h4(class="center","Graphical depiction of residuals"),
                                                    plotOutput(outputId = "regression_momentum_no_short_residual")
                                                  )
                                         )
                                       )
                              ),
                              tabPanel("Regression with shorting",
                                       wellPanel(
                                         tags$table(
                                           class = "center_table",
                                           tags$tr(
                                             tags$th("Intercept Estimate: "),
                                             tags$td(textOutput(outputId = "regression_momentum_short_intercept", inline = TRUE))
                                           ),
                                           tags$tr(
                                             tags$th("Slope Estimate: "),
                                             tags$td(textOutput(outputId = "regression_momentum_short_slope", inline = TRUE))
                                           ),
                                           tags$tr(
                                             tags$th("\\(\\mathbf R^2\\):"),
                                             tags$td(textOutput(outputId = "regression_momentum_short_r_square", inline = TRUE))
                                           )
                                         )
                                       ),
                                       tabsetPanel(
                                         tabPanel("Diagram of data with LS line",
                                                  wellPanel(
                                                    tags$h4(class="center","Regression of the strategy return on market return"),
                                                    plotOutput(outputId = "regression_momentum_short_LS")
                                                  )
                                         ),
                                         tabPanel("Residual Graph",
                                                  wellPanel(
                                                    tags$h4(class="center","Graphical depiction of residuals"),
                                                    plotOutput(outputId = "regression_momentum_short_residual")
                                                  )
                                         )
                                       )
                                )
                            )
                          )
                   ),
                   column(6,
                          wellPanel(
                            tags$h3(class="center", "Simple Moving Average Strategy"),
                            tabsetPanel(
                              tabPanel("Regression without shorting",
                                       wellPanel(
                                         tags$table(
                                           class = "center_table",
                                           tags$tr(
                                             tags$th("Intercept Estimate: "),
                                             tags$td(textOutput(outputId = "regression_ma_no_short_intercept", inline = TRUE))
                                           ),
                                           tags$tr(
                                             tags$th("Slope Estimate: "),
                                             tags$td(textOutput(outputId = "regression_ma_no_short_slope", inline = TRUE))
                                           ),
                                           tags$tr(
                                             tags$th("\\(\\mathbf R^2\\):"),
                                             tags$td(textOutput(outputId = "regression_ma_no_short_r_square", inline = TRUE))
                                           )
                                         )
                                       ),
                                       tabsetPanel(
                                         tabPanel("Diagram of data with LS line",
                                                  wellPanel(
                                                    tags$h4(class="center","Regression of the strategy return on market return"),
                                                    plotOutput(outputId = "regression_ma_no_short_LS")
                                                  )
                                         ),
                                         tabPanel("Residual Graph",
                                                  wellPanel(
                                                    tags$h4(class="center","Graphical depiction of residuals"),
                                                    plotOutput(outputId = "regression_ma_no_short_residual")
                                                  )
                                         )
                                       )
                              ),
                              tabPanel("Regression with shorting",
                                       wellPanel(
                                         tags$table(
                                           class = "center_table",
                                           tags$tr(
                                             tags$th("Intercept Estimate: "),
                                             tags$td(textOutput(outputId = "regression_ma_short_intercept", inline = TRUE))
                                           ),
                                           tags$tr(
                                             tags$th("Slope Estimate: "),
                                             tags$td(textOutput(outputId = "regression_ma_short_slope", inline = TRUE))
                                           ),
                                           tags$tr(
                                             tags$th("\\(\\mathbf R^2\\):"),
                                             tags$td(textOutput(outputId = "regression_ma_short_r_square", inline = TRUE))
                                           )
                                         )
                                       ),
                                       tabsetPanel(
                                         tabPanel("Diagram of data with LS line",
                                                  wellPanel(
                                                    tags$h4(class="center","Regression of the strategy return on market return"),
                                                    plotOutput(outputId = "regression_ma_short_LS")
                                                  )
                                         ),
                                         tabPanel("Residual Graph",
                                                  wellPanel(
                                                    tags$h4(class="center","Graphical depiction of residuals"),
                                                    plotOutput(outputId = "regression_ma_short_residual")
                                                  )
                                         )
                                       )
                              )
                            )
                          )
                   )
                 )
               )
             )
    )
  )
)






server<-function(input,output){
  #convert all the words you type to uppercase
  symbol_one<-reactive({toupper(input$symbol_one)})
  symbol_two_1<-reactive({toupper(input$symbol_two_1)})
  symbol_two_2<-reactive({toupper(input$symbol_two_2)})
  symbol_ma<-reactive({toupper(input$symbol_ma)})
  
  #complete "Histogram" part
  #compute mean and variance
  log_return_one<-reactive({get_StockPrice(symbol_one(),input$dates_one[1],input$dates_one[2])})
  mean_one<-reactive({round(mean(log_return_one()),10)})
  variance_one<-reactive({round(var(log_return_one()),10)})
  output$mean_one<-reactive({toString(mean_one())})
  output$variance_one<-reactive({toString(variance_one())})
  #plot the histogram for one symbol, adding the normal probability curve
  histogram_one<-reactive({plot_hist(symbol_one(), log_return_one())})
  output$histogram_one <- renderPlot(histogram_one())
  output$norm_pvalue<-reactive({normality(log_return_one())[1]})
  output$norm_result<-reactive({normality(log_return_one())[2]})
  
  #complete "Regression" part
  #first plot LS fitting line with data graph
  regression_one_model<-reactive({regression_one(log_return_one())})
  regression_one_LS <- reactive({
    par(bg=rgb(199,180,167,maxColorValue=255))
    plot(time(log_return_one()), log_return_one(), 
         xlab = 'time', 
         ylab = paste(symbol_one(), ' log-return'),
         main = "Diagram of data with LS line")
    abline(regression_one_model(), col=rgb(230, 180, 80, maxColorValue = 255), lwd = 3)
  })
  output$regression_LS_one<-renderPlot({regression_one_LS()})
  
  #next plot residual graph
  regression_one_residual<-reactive({
    par(bg=rgb(199,180,167,maxColorValue=255))
    plot(regression_one_model(),
         which=1)
  })
  output$regression_residual_one<-renderPlot({regression_one_residual()})
  
  #next complete related statistics part
  regression_summary_one<-reactive({summary(regression_one_model())})
  output$regression_intercept_one<-reactive({toString(round(regression_one_model()$coefficients[1],10))})
  output$regression_slope_one<-reactive({toString(round(regression_one_model()$coefficients[2],10))})
  output$regression_r_square_one<-reactive({round(regression_summary_one()$r.squared,10)})
  
  #complete "Hypothesis Testing" part
  alpha_temp<-reactive({input$alpha_in_one})
  alpha_one<-reactive({100*(1-alpha_temp())})
  output$alpha_out_mean_one<-reactive({toString(alpha_one())})
  output$alpha_out_variance_one<-reactive({toString(alpha_one())})
  ci<-reactive({confidence_intervals(symbol_one(),log_return_one(),alpha_temp())})
  mean_lower_one<-reactive({round(ci()$mean_lower,10)})
  mean_upper_one<-reactive({round(ci()$mean_upper,10)})
  variance_lower_one<-reactive({round(ci()$var_lower,10)})
  variance_upper_one<-reactive({round(ci()$var_lower,10)})
  output$mean_lower_one<-reactive({mean_lower_one()})
  output$mean_upper_one<-reactive({mean_upper_one()})
  output$variance_lower_one<-reactive({variance_lower_one()})
  output$variance_upper_one<-reactive({variance_upper_one()})
  
  #complete two symbols equality test
  log_return_two_1<-reactive({get_StockPrice(symbol_two_1(),input$dates_two[1],input$dates_two[2])})
  log_return_two_2<-reactive({get_StockPrice(symbol_two_2(),input$dates_two[1],input$dates_two[2])})
  alpha_two<-reactive({input$alpha_in_two})
  equal_test_summary<-reactive({equal_test(log_return_two_1(),log_return_two_2(),alpha_two())})
  equal_test_summary_pvalue<-reactive({equal_test_summary()$pvalue})
  equal_test_summary_conclusion<-reactive({equal_test_summary()$conclusion})
  output$pvalue<-reactive({toString(equal_test_summary_pvalue())})
  output$conclusion<-reactive({toString(equal_test_summary_conclusion())})
  
  #complete two symbols regression
  regression_two_model<-reactive({regression_two(log_return_two_1(),log_return_two_2())})
  regression_two_LS <- reactive({
    par(bg=rgb(199,180,167,maxColorValue=255))
    plot.zoo(as.ts(log_return_two_1()), as.ts(log_return_two_2()), 
             type="p",
         xlab = paste(symbol_two_1(), ' log-return'), 
         ylab = paste(symbol_two_2(), ' log-return'),
         main = "Diagram of data with LS line")
    abline(regression_two_model(), col=rgb(230, 180, 80, maxColorValue = 255), lwd = 3)
  })
  output$regression_LS_two<-renderPlot({regression_two_LS()})
  
  #next plot residual graph
  regression_two_residual<-reactive({
    par(bg=rgb(199,180,167,maxColorValue=255))
    plot(regression_two_model(),
         which=1)
  })
  output$regression_residual_two<-renderPlot({regression_two_residual()})

  #next complete related statistics part
  regression_summary_two<-reactive({summary(regression_two_model())})
  output$regression_intercept_two<-reactive({toString(round(regression_two_model()$coefficients[1],10))})
  output$regression_slope_two<-reactive({toString(round(regression_two_model()$coefficients[2],10))})
  output$regression_r_square_two<-reactive({round(regression_summary_two()$r.squared,10)})
  
  #complete momentum strategy
  
  return_momentum_no_short<-reactive({momentum(input$observe, input$hold, 0, input$dates_momentum[1], input$dates_momentum[2])})
  return_momentum_short<-reactive({momentum(input$observe, input$hold, 1, input$dates_momentum[1], input$dates_momentum[2])})

  
  regression_momentum_no_short_model<-reactive({regression_two(return_momentum_no_short()$market,return_momentum_no_short()$win)})
  regression_momentum_short_model<-reactive({regression_two(return_momentum_short()$market,return_momentum_short()$both)})
  
  regression_momentum_no_short_LS <- reactive({
    par(bg=rgb(199,180,167,maxColorValue=255))
    plot.zoo(as.ts(return_momentum_no_short()$market), as.ts(return_momentum_no_short()$win), 
             type="p",
             xlab = "market return", 
             ylab = "strategy return",
             main = "Diagram of data with LS line")
    abline(regression_momentum_no_short_model(), col=rgb(230, 180, 80, maxColorValue = 255), lwd = 3)
  })
  output$regression_momentum_no_short_LS<-renderPlot({regression_momentum_no_short_LS()})
  
  regression_momentum_short_LS <- reactive({
    par(bg=rgb(199,180,167,maxColorValue=255))
    plot.zoo(as.ts(return_momentum_short()$market), as.ts(return_momentum_short()$both), 
             type="p",
             xlab = "market return", 
             ylab = "strategy return",
             main = "Diagram of data with LS line")
    abline(regression_momentum_short_model(), col=rgb(230, 180, 80, maxColorValue = 255), lwd = 3)
  })
  output$regression_momentum_short_LS<-renderPlot({regression_momentum_short_LS()})
  
  #next plot residual graph
  regression_momentum_no_short_residual<-reactive({
    par(bg=rgb(199,180,167,maxColorValue=255))
    plot(regression_momentum_no_short_model(),
         which=1)
  })
  output$regression_momentum_no_short_residual<-renderPlot({regression_momentum_no_short_residual()})
  
  regression_momentum_short_residual<-reactive({
    par(bg=rgb(199,180,167,maxColorValue=255))
    plot(regression_momentum_short_model(),
         which=1)
  })
  output$regression_momentum_short_residual<-renderPlot({regression_momentum_short_residual()})
  
  
  #next complete related statistics part
  regression_summary_no_short_momentum<-reactive({summary(regression_momentum_no_short_model())})
  output$regression_momentum_no_short_intercept<-reactive({toString(round(regression_momentum_no_short_model()$coefficients[1],10))})
  output$regression_momentum_no_short_slope<-reactive({toString(round(regression_momentum_no_short_model()$coefficients[2],10))})
  output$regression_momentum_no_short_r_square<-reactive({round(regression_summary_no_short_momentum()$r.squared,10)})
  
  regression_summary_short_momentum<-reactive({summary(regression_momentum_short_model())})
  output$regression_momentum_short_intercept<-reactive({toString(round(regression_momentum_short_model()$coefficients[1],10))})
  output$regression_momentum_short_slope<-reactive({toString(round(regression_momentum_short_model()$coefficients[2],10))})
  output$regression_momentum_short_r_square<-reactive({round(regression_summary_short_momentum()$r.squared,10)})
  
  #now we come to the final part, simple moving average strategy

  return_ma_no_short<-reactive({MA_single(input$symbol_ma, 0, input$lag, input$dates_ma[1], input$dates_ma[2])})
  return_ma_short<-reactive({MA_single(input$symbol_ma, 1, input$lag, input$dates_ma[1], input$dates_ma[2])})
  
  cumreturn_ma_no_short<-reactive({cumreturn(return_ma_no_short())})
  cumreturn_ma_short<-reactive({cumreturn(return_ma_short())})
  
  regression_ma_no_short_model<-reactive({regression_one(cumreturn_ma_no_short())})
  regression_ma_short_model<-reactive({regression_one(cumreturn_ma_short())})
  
  regression_ma_no_short_LS <- reactive({
    par(bg=rgb(199,180,167,maxColorValue=255))
    plot(time(cumreturn_ma_no_short()), cumreturn_ma_no_short(), 
         xlab = 'time', 
         ylab = paste(symbol_ma(), ' cumulative return'),
         main = "Diagram of data with LS line")
    abline(regression_ma_no_short_model(), col=rgb(230, 180, 80, maxColorValue = 255), lwd = 3)
  })
  output$regression_ma_no_short_LS<-renderPlot({regression_ma_no_short_LS()})
  
  regression_ma_short_LS <- reactive({
    par(bg=rgb(199,180,167,maxColorValue=255))
    plot(time(cumreturn_ma_short()), cumreturn_ma_short(), 
         xlab = 'time', 
         ylab = paste(symbol_ma(), ' cumulative return'),
         main = "Diagram of data with LS line")
    abline(regression_ma_short_model(), col=rgb(230, 180, 80, maxColorValue = 255), lwd = 3)
  })
  output$regression_ma_short_LS<-renderPlot({regression_ma_short_LS()})
  
  #next plot residual graph
  regression_ma_no_short_residual<-reactive({
    par(bg=rgb(199,180,167,maxColorValue=255))
    plot(regression_ma_no_short_model(),
         which=1)
  })
  output$regression_ma_no_short_residual<-renderPlot({regression_ma_no_short_residual()})
  
  regression_ma_short_residual<-reactive({
    par(bg=rgb(199,180,167,maxColorValue=255))
    plot(regression_ma_short_model(),
         which=1)
  })
  output$regression_ma_short_residual<-renderPlot({regression_ma_short_residual()})
  
  #next complete related statistics part
  regression_summary_ma_no_short<-reactive({summary(regression_ma_no_short_model())})
  regression_summary_ma_short<-reactive({summary(regression_ma_short_model())})
  
  output$regression_ma_no_short_intercept<-reactive({toString(round(regression_ma_no_short_model()$coefficients[1],10))})
  output$regression_ma_no_short_slope<-reactive({toString(round(regression_ma_no_short_model()$coefficients[2],10))})
  output$regression_ma_no_short_r_square<-reactive({round(regression_summary_ma_no_short()$r.squared,10)})
  
  output$regression_ma_short_intercept<-reactive({toString(round(regression_ma_short_model()$coefficients[1],10))})
  output$regression_ma_short_slope<-reactive({toString(round(regression_ma_short_model()$coefficients[2],10))})
  output$regression_ma_short_r_square<-reactive({round(regression_summary_ma_short()$r.squared,10)})
  

}



shinyApp(ui=ui,server=server)
