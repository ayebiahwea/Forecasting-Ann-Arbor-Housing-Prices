library(tseries)
library(sandwich)
library(car)
library(forecast)
library(moments)
library(lmtest)
# Get data and convert to time series
AA= data$aa
AA=ts(AA, start = c(1982,2), end = c(2017,4), frequency = 4)

# lOG of A, find the difference, and plot the growth rate.

lAA=log(AA)
gaa=diff(lAA,lag = 1)*100
ts.plot(gaa)

# summary stats of gaa

sumry= summary(gaa)
sumry

sdv=sd(gaa)
sdv

kurt.gaa=kurtosis(gaa)
kurt.gaa

skew.gaa= skewness(gaa)
skew.gaa


#acf and pacf for gaa over 15 lags

acf(gaa, lag.max = 15)
acf(gaa, lag.max = 15, type = "partial")

# Q-stats for the growth rate

Box.test(gaa, lag = 1, type ="Ljung")
Box.test(gaa, lag = 2, type ="Ljung")
Box.test(gaa, lag = 3, type ="Ljung")
Box.test(gaa, lag = 4, type ="Ljung")
Box.test(gaa, lag = 5, type ="Ljung")
Box.test(gaa, lag = 6, type ="Ljung")
Box.test(gaa, lag = 7, type ="Ljung")
Box.test(gaa, lag = 8, type ="Ljung")
Box.test(gaa, lag = 9, type ="Ljung")
Box.test(gaa, lag = 10, type ="Ljung")
Box.test(gaa, lag = 11, type ="Ljung")
Box.test(gaa, lag = 11, type ="Ljung")
Box.test(gaa, lag = 12, type ="Ljung")
Box.test(gaa, lag = 13, type ="Ljung")
Box.test(gaa, lag = 14, type ="Ljung")
Box.test(gaa, lag = 15, type ="Ljung")


# Ar(3) process
gaa.ar3.fit=arima(gaa, order = c(3,0,0))
coeftest(gaa.ar3.fit)

#Ar(4) Process
gaa.ar4.fit=arima(gaa, order = c(4,0,0))
coeftest(gaa.ar4.fit)


#Estimate the MA(5) Process

gaa.ma5.fit = arima(gaa,order = c(0,0,5))
coeftest(gaa.ma5.fit)


# Estimate the ARMA(4,2)
gaa.arma42 = arima(gaa,order = c(4,0,2))
coeftest(gaa.arma42)

# acf and pacf up to 13 lags of residuals of AR(3)
# Getting the Q-stats from 4 through 13
acf(gaa.ar3.fit$residuals,lag.max = 13)
acf(gaa.ar3.fit$residuals,lag.max = 13, type = "partial")

Box.test(gaa.ar3.fit$residuals, lag = 4, type = c("Ljung"), fitdf = 3)
Box.test(gaa.ar3.fit$residuals, lag = 5, type = c("Ljung"), fitdf = 3)
Box.test(gaa.ar3.fit$residuals, lag = 6, type = c("Ljung"), fitdf = 3)
Box.test(gaa.ar3.fit$residuals, lag = 7, type = c("Ljung"), fitdf = 3)
Box.test(gaa.ar3.fit$residuals, lag = 8, type = c("Ljung"), fitdf = 3)
Box.test(gaa.ar3.fit$residuals, lag = 9, type = c("Ljung"), fitdf = 3)
Box.test(gaa.ar3.fit$residuals, lag = 10, type =c("Ljung"), fitdf = 3)
Box.test(gaa.ar3.fit$residuals, lag = 11, type = c("Ljung"), fitdf = 3)
Box.test(gaa.ar3.fit$residuals, lag = 12, type = c("Ljung"), fitdf = 3)
Box.test(gaa.ar3.fit$residuals, lag = 13, type = c("Ljung"), fitdf = 3)


#acf and pacf up to 13 lags of residuals of AR(4)
# Getting the Q-stats from 4 through 13
acf(gaa.ar4.fit$residuals,lag.max = 13)
acf(gaa.ar4.fit$residuals,lag.max = 13, type = "partial")

Box.test(gaa.ar4.fit$residuals, lag = 5, type = c("Ljung"), fitdf = 4)
Box.test(gaa.ar4.fit$residuals, lag = 6, type = c("Ljung"), fitdf = 4)
Box.test(gaa.ar4.fit$residuals, lag = 7, type = c("Ljung"), fitdf = 4)
Box.test(gaa.ar4.fit$residuals, lag = 8, type = c("Ljung"), fitdf = 4)
Box.test(gaa.ar4.fit$residuals, lag = 9, type = c("Ljung"), fitdf = 4)
Box.test(gaa.ar4.fit$residuals, lag = 10, type = c("Ljung"), fitdf = 4)
Box.test(gaa.ar4.fit$residuals, lag = 11, type =c("Ljung"), fitdf = 4)
Box.test(gaa.ar4.fit$residuals, lag = 12, type = c("Ljung"), fitdf = 4)
Box.test(gaa.ar4.fit$residuals, lag = 13, type = c("Ljung"), fitdf = 4)


#acf and pacf up to 13 lags of residuals of MA(5)
# Getting the Q-stats from 4 through 13
acf(gaa.ma5.fit$residuals,lag.max = 13)
acf(gaa.ma5.fit$residuals,lag.max = 13, type = "partial")

Box.test(gaa.ma5.fit$residuals, lag = 6, type = c("Ljung"), fitdf = 5)
Box.test(gaa.ma5.fit$residuals, lag = 7, type = c("Ljung"), fitdf = 5)
Box.test(gaa.ma5.fit$residuals, lag = 8, type = c("Ljung"), fitdf = 5)
Box.test(gaa.ma5.fit$residuals, lag = 9, type = c("Ljung"), fitdf = 5)
Box.test(gaa.ma5.fit$residuals, lag = 10, type = c("Ljung"), fitdf =5)
Box.test(gaa.ma5.fit$residuals, lag = 11, type = c("Ljung"), fitdf =5)
Box.test(gaa.ma5.fit$residuals, lag = 12, type =c("Ljung"), fitdf = 5)
Box.test(gaa.ma5.fit$residuals, lag = 13, type = c("Ljung"), fitdf =5)


#
