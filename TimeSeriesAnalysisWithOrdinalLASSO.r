## ----install_libraries, echo=FALSE, results='hide', message=FALSE, warning=FALSE----
#install.packages("sqldf")
#install.packages("dummies")
#install.packages("forecast")
#install.packages("orderedLasso")
#install.packages("glmnet")
#install.packages("h2o")
#install.packages("addendum")
#install.packages("testthat")
#install.packages("assertr")
#install.packages("assertthat")

rm(list=ls())

library(sqldf) #for running sql on data frames
library(dummies) #for creating one-hot encoding
library(forecast) #for the Holt-Winters forecast filter
#library(orderedLasso)
library(glmnet) #for running regularized GLM
#library(h2o)
library(knitr) #for reproducible research, i.e., Markdown
library(testthat)
library(devtools)
library(Quandl)

options(scipen=999)
options(digits=9)


## ----set_globals, message=FALSE, warning=FALSE---------------------------
##########################
##Input Global Variables##
##########################

##########################
#Input the column name of the dependent variable to predict.
dependent.variable <- "CPI"
##########################

##########################
#Set the maximum lag for adjusting the variables in the data.
#each variable will get a new column for each lag, up to the maximum set here.
maxlag <- 5
##########################

##########################
#Input the column name that has the time increments in it, such as years, or year/months.
time.increment.variable <- "Date"
##########################

##########################
#Select whether to include plots with the arima, pre-whitening step
include.arima.plots <- TRUE
##########################

##########################
#Select whether to include cross correlation plots
include.cross.correlation.plots <- TRUE
##########################

##########################
#Select whether to include quartile to quartile (QQ) plots
include.QQ.plots <- TRUE
##########################

##########################
#Select whether to include biplot
show.biplot <- TRUE
##########################

##########################
#Number of periods for which to predict ahead
num.periods.predict <- 1
##########################

##########################
#Set working directory
#getwd()
#setwd()
##########################


## ----load_data, echo=TRUE, results='hide'--------------------------------

#Note: this process takes the data in descending order, with the most recent data at the
?Quandl
CPI <- Quandl("RATEINF/CPI_USA", api_key="DJGcfzQc5RYP1JSycMBv", collapse="annual", start_date="1960-12-31", end_date="2017-12-31", type="raw", order="asc", force_irregular=TRUE)

#Nonfinancial corporate business; short-term debt as a percentage of total debt, Annual
shortTermDebtToLongTerm <- Quandl("FED/FL104140006_A", api_key="DJGcfzQc5RYP1JSycMBv", collapse="annual", start_date="1960-12-31", end_date="2017-12-31", type="raw", order="asc", force_irregular=TRUE)

#Financial soundness indicator, households; debt as a percent of gross domestic product
debtToGDP <- Quandl("FED/FL010000336_Q", api_key="DJGcfzQc5RYP1JSycMBv",collapse="annual", start_date="1960-12-31", end_date="2017-12-31", type="raw", order="asc", force_irregular=TRUE)


GDP <- Quandl("FED/FU086902001_A", api_key="DJGcfzQc5RYP1JSycMBv", collapse="annual", start_date="1960-12-31", end_date="2017-12-31", type="raw", order="asc", force_irregular=TRUE)


m1Velocity <- Quandl("FRED/M1V", api_key="DJGcfzQc5RYP1JSycMBv",collapse="annual", start_date="1960-12-31", end_date="2017-12-31", type="raw", order="asc", force_irregular=TRUE)


m2Velocity <- Quandl("FRED/M2V", api_key="DJGcfzQc5RYP1JSycMBv", collapse="annual", start_date="1960-12-31", end_date="2017-12-31", type="raw", order="asc", force_irregular=TRUE)

head(CPI)
head(shortTermDebtToLongTerm)
head(debtToGDP)
head(GDP)
head(m1Velocity)
head(m2Velocity)

tail(CPI)
tail(shortTermDebtToLongTerm)
tail(debtToGDP)
tail(GDP)
tail(m1Velocity)
tail(m2Velocity)

str(CPI)
str(shortTermDebtToLongTerm)
str(debtToGDP)
str(GDP)
str(m1Velocity)
str(m2Velocity)

raw.ts <- cbind(m1Velocity, m2Velocity, GDP[-1,], debtToGDP[-1,], shortTermDebtToLongTerm[-1,], CPI[-1,])

raw.ts <- cbind(m1Velocity, m2Velocity, GDP[-1,], debtToGDP[-1,], shortTermDebtToLongTerm[-1,], CPI[-1,])

#save time increment vector
#time.increments <- unique(raw_data_dummies[,time.increment.variable])
#time.increments <- time.increments[sort.list(time.increments, decreasing=FALSE)]
time.increments <- raw.ts[,time.increment.variable]

rownames(raw.ts) <- raw.ts[,1]

data <- raw.ts[, !(names(raw.ts) %in% "Date")]

colnames(data) <- c("CPI","shortTermDebtToLongTerm","debtToGDP","GDP","m1Velocity","m2Velocity")

head(data)

SeriesData <- data

x <- SeriesData[, !(names(SeriesData) %in% dependent.variable)]
head(x)

#scale the dependent variable
x.scaled <- scale(x)

#Isolate dependent variable values, based on name given in global variable inputs above
y <- SeriesData[,dependent.variable]

#scale the dependent variable
y.scaled <- scale(y)


#save column names
x.colnames <- colnames(x)


## ------------------------------------------------------------------------
#i=20
num.cols <- length(x[1,])
#apply(x,1,function(x) sum(is.na(x)))
#str(x)
#?auto.arima
#generate ARIMA plots...intent is to get ARIMA parameters, rather than forecasts
x.arima.residuals = NULL
for (i in 1:num.cols){
  fit <- auto.arima(x.scaled[,i])
  pdf(file=paste("plots/arima_",x.colnames[i],".pdf",sep=""))
  if(include.arima.plots == TRUE){
     par(mar=c(8,4,2,2))
     plot(forecast(fit,h=maxlag), sub=paste(x.colnames[i]))
  }
  dev.off()
  #assemble a table of ARIMA residuals for use in cross-correlation analysis
  temp.resid <- resid(fit)
  x.arima.residuals <- as.matrix(cbind(x.arima.residuals, temp.resid))
}

#run arima transformation on the dependent variable
fit=NULL
fit <- auto.arima(y.scaled)
par(mar=c(8,4,2,2))
pdf(file=paste("plots/arima_",dependent.variable,".pdf",sep=""))
plot(forecast(fit,h=1), sub=paste(dependent.variable, sep=""))
dev.off()
y.arima.residuals <- resid(fit)


## ------------------------------------------------------------------------
if(include.QQ.plots == TRUE){
#check distributions of independent variables for normality
  for (i in 1:length(x.scaled[1,])){
    pdf(file=paste("plots/qqnorm_",x.colnames[i],".pdf",sep=""))
    qqnorm(x.arima.residuals[,i], main=paste(x.colnames[i]))
    dev.off()
  }


  #check dependent variable for normality
  pdf(file=paste("plots/qqnorm_",dependent.variable,".pdf",sep=""))
  qqnorm(y.arima.residuals, main=paste(dependent.variable,sep=""))
  dev.off()
}




## ------------------------------------------------------------------------
#i=2
##cross correlation analysis
#leading indicators in 'x' will have negative lag values for the most significant
#correlations in the chart.
#note: analysis is run on ARIMA residuals so as to pre-whiten the data
#i=1
dir.create("plots")
 if(include.cross.correlation.plots == TRUE){
    for (i in 1:length(x[1,])){
    pdf(file=paste("plots/ccf_",x.colnames[i],".pdf",sep=""))
    par(mar=c(5,7,4,2)) #set the margins so title does not get cut off
    ccf(x.arima.residuals[,i], y.arima.residuals, plot=TRUE, main=paste(x.colnames[i]), na.action = na.contiguous)
   dev.off()
  }
}

## ------------------------------------------------------------------------



#Reverse the order of the variables, as lags are calculated in the functions below assuming that the input data is in ascending order, with the most recent values at the top rows of the matrix, or lefmost position in a vector.
x <- apply(x, 2, rev)

y <- rev(y)

x.arima.residuals <- apply(x, 2, rev)

y.arima.residuals <- rev(y.arima.residuals)

time.increments <- rev(time.increments)
#function to add time lags to the predictor variables in a new matrix
#with NO concurrent effects (i.e., 0 lead or lag)
if(concurrent.effects==FALSE){


###Not used...merely kept here for reference###
    time_lag_matrix2 <- function (x, maxlag)
  {
    p = ncol(x)
    N = nrow(x) - maxlag - 1
    x_new_num_col = maxlag * p
    x_new = matrix(0, nrow = N, ncol = x_new_num_col)
    for (i in (1:N)) {
      x_temp = x[(i + 1):(i + maxlag), ]
      x_new[i, ] = as.vector(x_temp)
    }
    return(x_new)
  }
} else {
  #function to add time lags to the predictor variables in a new matrix
  #while allowing for concurrent effects (i.e., allowing for 0 lead or lag)
  time_lag_matrix2 <- function (x, maxlag) {
    p = ncol(x)
    N = nrow(x) - maxlag - 1
    #    x_new_num_col = maxlag * p + p #as a check
    x_new = NULL
    for (j in 1:p) {
      for (i in 1:(maxlag + 1)) {
        x_temp = x[(i):(N + i), j]
        x_new = cbind(x_new,as.vector(x_temp))
      }
    }
    return(x_new)
  }
}

###Not used...merely kept here for reference###
#function to create lagged prediction set, and extending it
#using Holt Winters forecast
#i=2
#j=3
if(concurrent.effects==FALSE){
  forecast_matrix <- function (x, maxlag)
  {
    p = ncol(x)
    N = nrow(x) - maxlag - 1
    x_new_num_col = maxlag * p
    x_new = matrix(0, nrow = N, ncol = x_new_num_col)
    for (i in (1:N)) {
      x_temp = x[(i + 1):(i + maxlag), ]
      x_new[i, ] = as.vector(x_temp)
    }
    return(x_new)
  }
} else {
  forecast_matrix <- function (x, maxlag)
  {
    p = ncol(x)
    N = nrow(x) - maxlag - 1
    x_new = NULL
    for (j in 1:p) {
      hw <- HoltWinters(x[,j], gamma=FALSE, beta=TRUE)
      hw.predict <- predict(hw, n.ahead=num.periods.predict)
      x.forecast <- as.vector(c(hw.predict, x[,j]))
      for (i in 1:(maxlag + 1)) {
        x_temp = x.forecast[i:((maxlag + i) - 1)]
        x_new = as.matrix(cbind(x_new,as.vector(x_temp)))
      }
    }
    return(x_new)
  }
}
#?ets

#function to create new column headers for the matrix of lagged variables
#j=1
if(concurrent.effects==FALSE){
  time_lag_colnames <- function (x, maxlag)
  {
    #i=1
    #j=1
    p = ncol(x)
    N = nrow(x) - maxlag - 1
    x.colnames <- colnames(x)
    x_new_names = NULL
    for (j in 1:p) {
      for (i in 1:maxlag) {
        x_new_names_temp = rbind(paste(x.colnames[j], "_lead", i, sep=""))
        x_new_names <- rbind(x_new_names,x_new_names_temp)
      }
    }
    return(x_new_names)
  }
} else {
  time_lag_colnames <- function (x, maxlag) {
    p = ncol(x)
    N = nrow(x) - maxlag - 1
    x.colnames <- colnames(x)
    x_new_names = NULL
    for (j in 1:p) {
      for (i in 1:(maxlag + 1)) {
        x_new_names_temp = rbind(paste(x.colnames[j], "_lead", i-1, sep=""))
        x_new_names <- rbind(x_new_names,x_new_names_temp)
      }
    }
    return(x_new_names)
  }
}

#generate the lagged matrix
x_new = time_lag_matrix(as.matrix(x), maxlag)

#transformed data for diagnostics...NOT for forecasting
x_new_whitened <- time_lag_matrix(as.matrix(x.arima.residuals), maxlag)

#generate forecast set
#forecast.set <- forecast_matrix(x, maxlag)

#calculate length of new 'x' variable set
x_new.length <- length(x_new[,1])

#adjust the target variable to match the reduced length of the lagged predictor variables
y_new <- as.vector(y[1:x_new.length])
y_new_whitened <- as.vector(y.arima.residuals[1:x_new.length])
y_new_whitened <- scale(y_new_whitened) #scale for comparability of beta values

#generate the new column names for the new lagged matrix
x_new_colnames <- time_lag_colnames(x, maxlag)

#export forecast set
#colnames(forecast.set) <- x_new_colnames
#write.csv(file="ForecastSet.csv", x=forecast.set)


#shorten time increment list (for row names) to new length
time.increments <- time.increments[1:length(y_new)]


## ----prewhitened_model---------------------------------------------------

####cross validated models on pre-whitened data for diagnostic purposes

cvfitlm.prewhitened <- timeLagLasso(x=x_new_whitened, y=y_new_whitened, lambda=1, maxlag=maxlag, method="Solve.QP", strongly.ordered=TRUE)
#?timeLagLasso

predict(cvfitlm.prewhitened)

cvfitlm <- timeLagLasso(x=x_new, y=y_new, lambda=1, maxlag=maxlag, method="GG", strongly.ordered=TRUE, iter.gg = 1000)

predict(cvfitlm)



#add the column names to the coefficient list
#first, must add a row for the y-intercept value in the column names list
x_coef_colnames <- rbind("Intercept",x_new_colnames)

model1 = timeLagLasso(x = x, y = y, lambda = 1, maxlag = maxlag, method = "Solve.QP", strongly.ordered = TRUE)

predict(model1)



