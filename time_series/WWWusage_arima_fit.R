## load libraries
library(car)
library(forecast)
library(rmarkdown)
library(graphics)
#check data
WWWusage

ts.plot(WWWusage ,ylab = 'WWW usage per minute')
#some visual analisys
dWWW <- diff(WWWusage)
plot(dWWW)
acf(dWWW)
pacf(dWWW)
#auto arima
f<- auto.arima(WWWusage); f
acf(WWWusage)

#try 1 - based on diff analisys + auto.arima(1,1,1)
f <- Arima(WWWusage, order = c(1,1,1)) ; f
acf(f$residuals)
#try 2 - increasing diff and p
f <- Arima(WWWusage, order = c(2,2,1)) ; f
acf(f$residuals) #looks good


#try 3 - inputing zero in phi3
f <- Arima(WWWusage, order = c(2,2,1), fixed=c(NA,NA,0) ) ; f
acf(f$residuals) #looks good
#check if remains auto correlation
Box.test(f$residuals, lag=15, fitdf=4)
#Check residual distribuition
qqPlot(f$residuals) #ok

#Visualize forecast
plot(forecast(f,10))                   
lines(fitted(f), col=2)






