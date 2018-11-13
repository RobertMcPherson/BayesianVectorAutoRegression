## ----global_options, include=FALSE---------------------------------------
#knitr::opts_chunk$set(include=TRUE, warning=FALSE, message=FALSE,echo=FALSE)

## ----install_libraries, results='hide'-----------------------------------
#install.packages("sqldf")
#install.packages("dummies")
#install.packages("forecast")
#install.packages("orderedLasso")
#install.packages("glmnet")
#install.packages("h2o")
#install.packages("addendum")
#install.packages("testthat")
#devtools::use_testthat

rm(list=ls())

#setwd("R:/AnalyticsTeam/Personal/May/BitBucket/Gemini Time Series")
#setwd("C:/Users/rmcpherson/Documents/Segments/Phil Welt Segment/Gemini")

library(sqldf) #for running sql on data frames
library(dummies) #for creating one-hot encoding
library(forecast) #for the Holt-Winters forecast filter
library(glmnet) #for running regularized GLM
library(knitr) #for reproducible research, i.e., Markdown
#library(testthat)
library(BigVAR)
library(orderedLasso)
library(reshape)
library(ggplot2)
library(Quandl)


## ----set_globals---------------------------------------------------------

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
maxlag <- 12
##########################

##########################
#Type 'TRUE' if you want to include an offset in the GLM calculation, FALSE otherwise.
include.offset <- FALSE
##########################

##########################
#Type the column name of the variable you would like to use as an offset, if any.
#offset.variable <- "UnitCount"
##########################

##########################
#Input the column name that has the time increments in it, such as years, or year/months.
time.increment.variable <- "Date"
##########################

##########################
#Select whether to include plots with the arima, pre-whitening step
include.arima.plots <- FALSE
##########################

##########################
#Select whether to include cross correlation plots
include.cross.correlation.plots <- TRUE
##########################

##########################
#Select whether to include quartile to quartile (QQ) plots
include.QQ.plots <- FALSE
##########################


## ----load_data, results='hide'-------------------------------------------


#Note: this process takes the data in descending order, with the most recent data at the
CPI <- Quandl("RATEINF/CPI_USA", api_key="DJGcfzQc5RYP1JSycMBv", collapse="annual", start_date="1960-12-31", end_date="2017-12-31", type="raw", order="asc", force_irregular=TRUE)

#Nonfinancial corporate business; short-term debt as a percentage of total debt, Annual
shortTermDebtToLongTerm <- Quandl("FED/FL104140006_A", api_key="DJGcfzQc5RYP1JSycMBv", collapse="annual", start_date="1960-12-31", end_date="2017-12-31", type="raw", order="asc", force_irregular=TRUE)

#Financial soundness indicator, households; debt as a percent of gross domestic product
debtToGDP <- Quandl("FED/FL010000336_Q", api_key="DJGcfzQc5RYP1JSycMBv",collapse="annual", start_date="1960-12-31", end_date="2017-12-31", type="raw", order="asc", force_irregular=TRUE)


GDP <- Quandl("FED/FU086902001_A", api_key="DJGcfzQc5RYP1JSycMBv", collapse="annual", start_date="1960-12-31", end_date="2017-12-31", type="raw", order="asc", force_irregular=TRUE)


m1Velocity <- Quandl("FRED/M1V", api_key="DJGcfzQc5RYP1JSycMBv",collapse="annual", start_date="1960-12-31", end_date="2017-12-31", type="raw", order="asc", force_irregular=TRUE)


m2Velocity <- Quandl("FRED/M2V", api_key="DJGcfzQc5RYP1JSycMBv", collapse="annual", start_date="1960-12-31", end_date="2017-12-31", type="raw", order="asc", force_irregular=TRUE)

#head(CPI)
#head(shortTermDebtToLongTerm)
#head(debtToGDP)
#head(GDP)
#head(m1Velocity)
#head(m2Velocity)

#tail(CPI)
#tail(shortTermDebtToLongTerm)
#tail(debtToGDP)
#tail(GDP)
#tail(m1Velocity)
#tail(m2Velocity)

#str(CPI)
#str(shortTermDebtToLongTerm)
#str(debtToGDP)
#str(GDP)
#str(m1Velocity)
#str(m2Velocity)

raw.ts <- cbind(m1Velocity, m2Velocity, GDP[-1,], debtToGDP[-1,], shortTermDebtToLongTerm[-1,], CPI[-1,])

raw.ts <- cbind(m1Velocity, m2Velocity, GDP[-1,], debtToGDP[-1,], shortTermDebtToLongTerm[-1,], CPI[-1,])

#save time increment vector
#time.increments <- unique(raw_data_dummies[,time.increment.variable])
#time.increments <- time.increments[sort.list(time.increments, decreasing=FALSE)]
time.increments <- raw.ts[,time.increment.variable]

rownames(raw.ts) <- raw.ts[,1]

data <- raw.ts[, !(names(raw.ts) %in% "Date")]

colnames(data) <- c("CPI","shortTermDebtToLongTerm","debtToGDP","GDP","m1Velocity","m2Velocity")

#head(data)

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


## ----ARIMA, results='asis'-----------------------------------------------
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


## ----analytical_dataset, results='hide'----------------------------------

#x
#y
#x.arima.residuals <- apply(x.arima.residuals, 2, rev)
#y.arima.residuals <- rev(y.arima.residuals)
#time.increments <- rev(time.increments)

#x2 <- subset(x, select= -c(X.Dividends, X.From.the.rest.of.the.world.6.))

Y <- cbind.data.frame(x, "CPI" = y)
nms <- colnames(Y)
Y <- as.matrix(Y)

# Fit a Basic VAR-L(3,4) on simulated data
T1=floor(nrow(Y)/3)
T2=floor(2*nrow(Y)/3)
#?constructModel
#m1=constructModel(Y,p=4,struct="Basic",gran=c(20,10),verbose=FALSE,IC=FALSE,T1=T1,T2=T2,ONESE=TRUE)
#m1=constructModel(Y,p=4,struct="Tapered",gran=c(50,10),verbose=FALSE,T1=T1,T2=T2,IC=FALSE)
#plot(m1)
#results=cv.BigVAR(m1)
#plot(results)
#predict(results,n.ahead=1)

#SparsityPlot.BigVAR.results(results)

#str(results)
#results@preds
#results@alpha
#results@Granularity
#results@Structure
#results@lagmax
#results@Data
#plot(results@Data)

#install.packages("devtools")
#library(devtools)
#install_github("gabrielrvsc/HDeconometrics")

###################################
#The above, BigVAR package will not handle data sets this wide.  Trying the
#Bayesian Vector Auto Regression (BVAR) algorithm

# = load package and data = #
library(HDeconometrics)
#data("voldata")

# = Break data into in and out of sample to test model accuracy= #
#Yin=voldata[1:5499,]
#Yout=voldata[-c(1:5499),]
Yin = Y[1:T2,]
Yout = Y[(T2+1):(T1+T2),]

# = Run models = #
# = OLS = #
#modelols=HDvar(Yin,p=2) # takes a while to run
#predols=predict(modelols,h=2)

# = BVAR = #
modelbvar=lbvar(Yin, p = 2, delta = 0.5)
predbvar=predict(modelbvar,h=2)

# = Forecasts of the volatility = #
#?tail
k="CPI"
pdf(file=paste("plots/forecast.pdf",sep=""))
plot(c(Y[,k],predbvar[,k]),type="l", main="CPI Forecast")
#lines(c(rep(NA,length(Y[,k])),predols[,k]))
lines(c(rep(NA,length(Y[,k])),predbvar[,k]))
abline(v=length(Y[,k]),lty=2,col=4)
#legend("topleft",legend="BVAR",col=2,lty=1,lwd=1,seg.len=1,cex=1,bty="n")
dev.off()

# = Overall percentual error = #
#MAPEols=abs((Yout-predols)/Yout)*100
#MAPEbvar=abs((Yout-predbvar)/Yout)*100
#matplot(MAPEols,type="l",ylim=c(0,80),main="Overall % error",col="lightsalmon",ylab="Error %")
#aux=apply(MAPEbvar,2,lines,col="lightskyblue1")
#lines(rowMeans(MAPEols),lwd=3,col=2,type="b")
#lines(rowMeans(MAPEbvar),lwd=3,col=4,type="b")
#legend("topleft",legend=c("OLS","BVAR"),col=c(2,4),lty=1,lwd=1,seg.len=1,cex=1,bty="n")

# = Influences = #
#aux=modelbvar$coef.by.block[2:23]
#impacts=abs(Reduce("+", aux ))
#diag(impacts)=0
#I=colSums(impacts)
#R=rowSums(impacts)
#par(mfrow=c(2,1))
#barplot(I,col=rainbow(30),cex.names = 0.3, main = "Most Influent")
#barplot(R,col=rainbow(30),cex.names = 0.3, main = "Most Influenced")

pdf(file=paste("plots/barchart.pdf",sep=""))
aux=modelbvar$coef.by.block
impacts=abs(Reduce("+", aux ))
diag(impacts)=0
I=colSums(impacts)
R=rowSums(impacts)
par(mfrow=c(2,1))
barplot(I,col=rainbow(30),cex.names = 0.3, main = "Most Influent")
barplot(R,col=rainbow(30),cex.names = 0.3, main = "Most Influenced")
dev.off()



