setwd("C://Sem2/SDM/R/")
nike = read.csv("NKE (1).csv")
View(nike)
nrow(nike)
#Studying the P Values

M1 = lm(data=nike,Volume~nike$Open+nike$High+nike$Low+nike$Close+nike$Adj.Close)
summary(M1)

plot(x = nike$Date,y = nike$Volume,data = nike)

ua = read.csv("UAA.csv")
View(ua)


adidas = read.csv("ADDYY.csv") # Reading Adidas
View(adidas)



library(quantmod)
library(xts)
library(rvest)
library(tidyverse)
library(stringr)
library(forcats)
library(lubridate)
library(ggplot2)
library(dplyr)
library(PerformanceAnalytics)
install.packages("quantmod")
install.packages("xts")
install.packages("rvest")
install.packages("tidyverse")
install.packages("stringr")
install.packages("forcats")
install.packages("lubridate")
install.packages("PerformanceAnalytics")
#install.library("xts")
#install.library("xts")

# Lets look at the trends in the Stock Market #

plot(nike[,5],type = "l",xlab = "Time Range",ylab = "Adjusted Close",main = "NIKE")

plot(ua[,5],type = "l",xlab = "Time Range", ylab = "Adjusted Close", main = "Under Armor") ## Plot of Under armor. 

## It appears that post the 600 days there is a decline in the Share price for Under Armor. 
## We want to know what went wrong during that time ? 

plot(adidas[,5],type = "l",xlab = "Time Range", ylab = "Adjusted Close", main = "Adidas")

## Summary Statistics for each of the variables : 

summary(nike)
summary(adidas)
summary(ua)

# From the summary I want to know which date gave the Maximum and Minmum share prices : 

boxplot(ua$Adj.Close) # Observe no outliers here
boxplot(nike$Adj.Close) # Outliers are present -- Big company , and for obvious reasons.  
boxplot(adidas$Adj.Close) # observe no outliers here

# Nike Analysis

## Lets get the Returns and Log Returns : 

n1=length(nike$Adj.Close)
n1
Ret1 = nike$Adj.Close[-1]/nike$Adj.Close[-n1]-1 # Returns
ret1 = diff(log(nike$Adj.Close)) # log returns
plot(x = nike$Date[-1],y = 100*Ret1,type = "l",xlab="Time",ylab="%",col=1)
lines(nike$Date[-1],100*Ret1,col ="green")

#points(nike$Date[-1],100*ret1,col=2,cex=0.1 )

plot(x = nike$Date[-1],y = 100*ret1,type = "l",xlab="Time",ylab="Log-Return",col=1)
lines(nike$Date[-1],10*ret1,col ="black")

#legend(16600,-4.5,legend=c("Ret","Log-Return"),lty=1,col=c(1:2))


summary(ret1)   # Summary For Log Returns
kurtosis(ret1) # Kurtosis
skewness(ret1)  # Skewness

##Spectrum Analysis

raw1 = spectrum(ret1)
smooth = spectrum(ret1,spans=c(25,5,25),main="Smoothed periodogram",ylim=c(5e-5,4e-4))
 
# From the smoothed periodogram, we can see that there is no significant dominant frequency, which means
# there is no significant cycles.Although there are some small peaks in the spectrum, but when we move the
# crossbar to each peak alongthe estimated spectrum, it gives pointwise 95% confidence intervals, and we 
# can see that all the peaks are insignificant.This is not contrary to our common sense that the stock 
# price and return is a kind like random walk, one can hardly find cycles in such a few years.


############ FITTING ARIMA Model

# Null Hypothesis : There is no Trend. 
# Alternate Hypothese : There is trend. 

# First we need to Decide which Auto Regressive Moving Average Model Needs to be used to answer ? 

# Code to print the AR MA values as a matrix. 

#https://rstudio-pubs-static.s3.amazonaws.com/345790_3c1459661736433382863ed19c30ea55.html
#https://www.quantstart.com/articles/Autoregressive-Moving-Average-ARMA-p-q-Models-for-Time-Series-Analysis-Part-3
#http://www-stat.wharton.upenn.edu/~stine/insr260_2009/lectures/arma_forc.pdf

# to be explaned by naman 
aic_table_nike = function(nike,P,Q){
  table_nike = matrix(NA,(P+1),(Q+1))
  for (p in 0:P){
    for (q in 0:Q){
      table_nike[p+1,q+1] = arima(nike,order=c(p,0,q))$aic
    }
  }
  dimnames(table_nike) = list(paste("<b> AR",0:P,"</b>",sep=""),paste("MA",0:Q,sep=""))
  table_nike
}

ret_aic_table_nike = aic_table_nike(ret1,4,4)

require(knitr)
kable(ret_aic_table_nike,digits=2)


## Here we need to choose the lowest AR vs MA value from the AIC table (Recall the learning).. 

min(ret_aic_table_nike) # This gives that the AR - 3 and MA - 3 is the best model.

# However, we also see that the AR1 vs MA1 is the third lowest. We will analyse for these two models. 

nike_arma33 = arima(ret1,order = c(4,0,4))
nike_arma33

nike_arma11 = arima(ret1,order = c(1,0,1))
nike_arma11

# We observe that the log likelihood of ARMA Model with p = 3 and q = 3 is the most apt model.
# Since the model with p = 3 and q = 3 happens to suffer with overfitting we choose the next best one 
# i.e p =1,q=1
# Hence we will try to model this for the moment. 
# the model looking at the parameter is : (1-0.7080B)(x(n)-0.0007) = (1-0.7733B)e(n)
# e(n) ~ N[0.2267]

# Confidence Interval for the parameters for nike : 
# [0.4324,0.9835]
# [-1.0085 , -.5381]

#### Test for Residuals : 

acf(resid(nike_arma11))

####Finally, we perform the Ljung-Box test for 20 lags to confirm this:

Box.test(resid(nike_arma11), lag=20, type="Ljung-Box")
## ACF of Residuals is near to zero which means that model is relatively suitable

##### Forecasting
library(quantmod)
library(fArma)
install.packages("fArma")


## partition into train and test
train_series=ret1[1:5000]
View(train_series)
test_series=ret1[5001:5033]

## make arima models
arima_ret1_1=arima(train_series, order=c(1,0,1))
arima_ret1_2=arima(train_series, order=c(3,0,3))

install.packages("forecast")
print(arima_nike_1)
print(arima_nike_2)

require(forecast)
future_101 = forecast(arima_ret1_1, h = 50)
future_303 = forecast(arima_ret1_2, h = 50)
plot(future_101,col = "orange")
plot(future_303,col = "blue")


## Use Predict here to check for any date - Pending

arima_nike_1 = arima(nike[,5],order = c(1,0,1))
print(arima_nike_1)
future_nike_101 = forecast(nike[,5],h=150)
plot(future_nike_101,col="purple")

ten_days = predict(arima_nike_1,n.ahead = 3)
ten_days

### the graph shows an upward trend and it appears that the stock price will take a further upward turn.
## Light Gray interval shows 99 % confidence interval. Dark grey shows 95% confidence interval. 


## Checking for Seasonality
# time series with strong Seasonality:
# What is seasonal Data ? - The one which has repetitive/Cyclic behaviour when observed in a graph.

install.packages("fpp")
library(fpp)

ts_nike = ts(nike, frequency = 4, start = 1999)
View(ts_nike)
decompose_nike = decompose(nike, "multiplicative") # Time series has no Periods and Arima can be applied.
#adjust_nike = ts_nike - decompose_nike$seasonal
#plot(adjust_nike)
