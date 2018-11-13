#install.packages("devtools")
#library(devtools)

#install_github("braverock/FinancialInstrument")
#library(FinancialInstrument)

#install_github("braverock/blotter")
#library(blotter)

#install_github("braverock/quantstrat")

#install_github("braverock/PerformanceAnalytics")

#install.packages("zoo")
library(quantstrat)

Sys.setenv(TZ = "Asia/Kolkata")
currency('INR')
init_date <- "2011-12-31" #the date on which we want to intialize the portfolio and account
start_date <- "2012-01-01" #the date from which we want to collect the data
end_date <- "2017-12-31" #the date untill when we want to collect the data
init_equity <- 100000 #initial account equity value
adjustment <- TRUE #TRUE when we want to adjust prices otherwise FALSE

getSymbols(Symbols = "^NSEI", src = "yahoo", from = start_date, to = end_date, adjust = adjustment)
knitr::kable(head(NSEI))

#install.packages("plotly")
library(plotly)
df <- data.frame(Date=index(NSEI),coredata(NSEI))
plot_ly(x = df$Date, type="candlestick",
        open = df$NSEI.Open, close = df$NSEI.Close,
        high = df$NSEI.High, low = df$NSEI.Low)

######################################
## Tutorial Part II
######################################

library(quantstrat)
Sys.setenv(TZ = "Asia/Kolkata")
currency('INR')

init_date <- "2011-12-31"
start_date <- "2012-01-01"
end_date <- "2017-12-31"
init_equity <- 100000
adjustment <- TRUE

getSymbols(Symbols = "^NSEI", 
           src = "yahoo", 
           from = start_date, 
           to = end_date, 
           adjust = adjustment)

stock("NSEI",currency="INR",multiplier = 1)

strategy.st<-"basic_strat"
portfolio.st<-"basic_portfolio"
account.st<-"basic_account"

rm.strat(portfolio.st)
rm.strat(account.st)

initPortf(name = portfolio.st,symbols = "NSEI",initDate = init_date)

initAcct(name = account.st,portfolios = portfolio.st,initDate = init_date,initEq = init_equity)

initOrders(portfolio = portfolio.st,symbols = "NSEI",initDate = init_date)

strategy(strategy.st, store = TRUE)

#SMA
color="red"
chartSeries(NSEI$NSEI.Close,TA="addSMA(n=40,col=color)") 

#MACD
chartSeries(NSEI$NSEI.Close, TA="addMACD(fast = 12, slow = 26, signal = 9, histogram = TRUE)")

#RSI
chartSeries(NSEI$NSEI.Close, TA="addRSI(n=14)")

#Bollinger Bands
sma="SMA"
bands="bands"
color="blue"
chartSeries(NSEI$NSEI.Close, TA="addBBands(n=20,maType=sma,draw=bands)")

#40 Day SMA
add.indicator(strategy.st, name = "SMA", 
              arguments = list(x=quote(Cl(mktdata)),n=40),
              label='SMA_40' )

#7 Day RSI
add.indicator(strategy.st, name = "RSI", 
              arguments = list(x=quote(Cl(mktdata)),n=7),
              label='RSI_7' )

######################################
## Tutorial Part III
######################################

#In this tutorial we will learn how to add indicators and signals to the strategy. As this is an introduction tutorial and to keep it simple, we will build strategy for ‘long trade'(discussed in chapter 1) only i.e., we buy first and later sell.
#The strategy that we are going to use in this tutorial is as follows:
#Enter the market when:
#RSI7 value is greater than a threshold value of 50 & MACD histogram crosses the 0-line from below
#Exit the market when :
#either RSI7 value is lesser than a threshold value of 50 or MACD histogram crosses the 0-line from above.

library(quantstrat)
Sys.setenv(TZ = "Asia/Kolkata")
currency('INR')

init_date <- "2011-12-31"
start_date <- "2012-01-01"
end_date <- "2017-12-31"
init_equity <- 100000
adjustment <- TRUE
getSymbols(Symbols = "^NSEI", 
           src = "yahoo", index.class = "POSIXct",
           from = start_date, 
           to = end_date, 
           adjust = adjustment)

knitr::kable(head(NSEI))

NSEI=na.omit(NSEI)
stock("NSEI",currency="INR",multiplier = 1)

strategy.st<-"basic_strat"
portfolio.st<-"basic_portfolio"
account.st<-"basic_account"
rm.strat(portfolio.st)
rm.strat(account.st)
initPortf(name = portfolio.st,symbols = "NSEI",initDate = init_date)

initAcct(name = account.st,portfolios = portfolio.st,initDate = init_date,initEq = init_equity)

initOrders(portfolio = portfolio.st,symbols = "NSEI",initDate = init_date)
strategy(strategy.st, store = TRUE)

#RSI indicator
add.indicator(strategy = strategy.st,
              name = "RSI",
              arguments = list(price = quote(Cl(mktdata)), 
                               n = 7),
              label = "RSI_7")

chartSeries(RSI(NSEI$NSEI.Close,n=7),theme="black",name="RSI n=7")
abline(a=50,b=0,col="blue")

#MACD indicator
fastMA = 12 
slowMA = 26 
signalMA = 9
maType="EMA"
add.indicator(strategy.st, name = "MACD", 
              arguments = list(x=quote(Cl(mktdata)),
                               nFast=fastMA, 
                               nSlow=slowMA,histogram = TRUE),
              label='MACD' 
)

chartSeries(NSEI$NSEI.Close,TA="addMACD(fast = 12, slow = 26, signal = 9)",theme="black",name="MACD 26-12-9")


mktdata_ind <- applyIndicators(strategy=strategy.st,mktdata=NSEI)
mktdata_ind[is.na(mktdata_ind)]=0
knitr::kable(tail(mktdata_ind))

#Add signal for RSI threshold of greater than 50
add.signal(strategy.st, name = "sigThreshold", arguments = list(column = "rsi.RSI_7",threshold=50,relationship="gt"), label = "RSI_gt_50")

#MACD histogram is the difference between the macd line and the signal line, when macd 
#histogram crosses zero line from below, macd line crosses the signal line from below and 
#macd value will be greater than signal value.
#Using add.signal function add a signal macd_gt_0 to the strategy which returns TRUE when 
#macd.MACD crosses the signal.MACD. relationship="gt" since we want the points where macd 
#crosses the signal from below. To generate the signal use sigCrossover function as we are 
#comparing 2 indicators.
add.signal(strategy.st, name = "sigCrossover", arguments = list(columns = c("macd.MACD","signal.MACD"),relationship="gt"), label = "macd_gt_0")

#Using add.signal function add a signal Long to the strategy which returns TRUE when 
#RSI_gt_50 & macd_gt_0 are True. To generate the signal use sigFormula function as we 
#are evaluating a logical expression to generate a signal.
add.signal(strategy.st, name = "sigFormula",
           arguments = list(formula="RSI_gt_50 & macd_gt_0",
                            cross = FALSE), label = "Long")

#Using add.signal function add a signal RSI_lt_50 to the strategy which returns TRUE 
#when rsi.RSI_7 is less than 50. To generate the signal use sigThreshold function as we are 
#comparing indicator to a threshold value.
add.signal(strategy.st, name = "sigThreshold", arguments = list(column = "rsi.RSI_7",threshold=50,relationship="lt"), label = "RSI_lt_50")

#MACD histogram is the difference between the macd line and the signal line, when macd histogram 
#crosses zero line from above, macd line crosses the signal line from above and macd value 
#will be less than signal value.
#Using add.signal function add a signal macd_lt_0 to the strategy which returns TRUE when 
#macd.MACD crosses the signal.MACD. relationship="lt" since we want the points where macd 
#crosses the signal from above. To generate the signal use sigCrossover function as we are 
#comparing 2 indicators.
add.signal(strategy.st, name = "sigCrossover", arguments = list(columns = c("macd.MACD","signal.MACD"),relationship="lt"),cross=TRUE, label = "macd_lt_0")

#applySignals function in R is used to apply the signals to the strategy based on indicators. 
#The mktdata is the ouput data obtained after applying indicators to the strategy i.e., mktdata_ind 
#in this case.
mktdata_sig <- applySignals(strategy=strategy.st,mktdata=mktdata_ind)
mktdata_sig[is.na(mktdata_sig)]=0
knitr::kable(tail(mktdata_sig))

######################################
## Tutorial Part IV
######################################

#Enter long
add.rule(strategy = strategy.st, name="ruleSignal", arguments = list(
  sigcol = "Long", sigval = TRUE, orderqty = 100, ordertype = "market",TxnFees = -75, 
  orderside = "long",prefer="Open", replace = FALSE), type = "enter",label='enter long')

#Exit long rule 1
add.rule(strategy.st,name='ruleSignal', 
         arguments = list(sigcol="macd_lt_0",
                          sigval=TRUE, 
                          orderqty='all', 
                          ordertype='market', 
                          orderside='long', 
                          prefer="Open",
                          TxnFees = -75,
                          replace=TRUE),
         type='exit',
         label='long exit1'
         
)

#Exit long rule 2
add.rule(strategy.st,name='ruleSignal', 
         arguments = list(sigcol="RSI_lt_50",
                          sigval=TRUE, 
                          orderqty='all', 
                          ordertype='market', 
                          orderside='long', 
                          prefer="Open",
                          TxnFees = -75,orderset='ocolong',
                          replace=TRUE),
         type='exit',
         label='long exit2'
)

#Apply strategy
applyStrategy(strategy = strategy.st,portfolios = portfolio.st)

#Calculate PL
updatePortf(portfolio.st)

#Update account
updateAcct(account.st)

#Ending equity and net performance
updateEndEq(account.st)

#Chart trades
chart.Posn(portfolio.st,"NSEI")

######################################
## Tutorial Part V
######################################

#Chart trades
chart.Posn(portfolio.st,"NSEI")

#Trade statistics - all
trade_stats <- tradeStats(portfolio.st)
trade_stats1 <- as.data.frame(t(tradeStats(portfolio.st)))
knitr::kable(trade_stats1)

##Run statistics by type, separately instead of all at once...

#Trade statistics - just basic stats
knitr::kable(trade_stats1[c("Portfolio","Symbol","Num.Txns","Num.Trades"),])

#Profit and loss
knitr::kable(trade_stats1[c("Net.Trading.PL","Avg.Trade.PL","Med.Trade.PL","Std.Dev.Trade.PL","Std.Err.Trade.PL","Avg.Daily.PL","Med.Daily.PL","Std.Dev.Daily.PL","Std.Err.Daily.PL"),])

#Sharpe ratio
knitr::kable(trade_stats1[c("Ann.Sharpe"),])

#Performance summary charts
returns<-PortfReturns(Account=account.st)
charts.PerformanceSummary(returns,colorset=bluefocus)

#Per trade stats
knitr::kable(head(perTradeStats(portfolio.st, Symbol = "NSEI")))




























