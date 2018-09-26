rm(list=ls())

install.packages("vars")

set.seed(123) # Reset random number generator for reasons of reproducability
# Generate sample
t <- 200 # Number of time series observations
k <- 2 # Number of endogenous variables
p <- 2 # Number of lags
A.1 <- matrix(c(-.3,.6,-.4,.5),k) # Coefficient matrix of lag 1
A.2 <- matrix(c(-.1,-.2,.1,.05),k) # Coefficient matrix of lag 2
A <- cbind(A.1,A.2) # Companion form of the coefficient matrices

series <- matrix(0,k,t+2*p) # Raw series with zeros
for (i in (p+1):(t+2*p)){ # Generate series with e ~ N(0,0.5)
series[,i] <- A.1%*%series[,i-1] + A.2%*%series[,i-2] + rnorm(k,0,.5)
}

series <- ts(t(series[,-(1:p)])) # Convert to time series format
names <- c("V1","V2") # Rename variables
plot.ts(series) # Plot the series

# Run analysis
library(vars) # Load package
var.1 <- VAR(series,2,type="none") # Estimate the model

var.aic <- VAR(series,type="none",lag.max=5,ic="AIC")
summary(var.aic)

A # True values
round(rbind(coef(var.aic)[[1]][,1],coef(var.aic)[[2]][,1]),2) # Rounded estimates

# Impulse responses
ir.1 <- irf(var.1,impulse="Series.1",response="Series.2",n.ahead = 20,ortho = FALSE)
plot(ir.1)

ir.2 <- irf(var.1,impulse="Series.1",response="Series.2",n.ahead = 20,ortho = FALSE,
cumulative = TRUE)
plot(ir.2)




