## load libraries
library(car)
library(forecast)
library(rmarkdown)
#load data
anfavea.data <- read.csv2('time_series/data/automoveismontadosanfavea.csv') 
colnames(anfavea.data) <- c('dates', 'auto_production', 'x')
anfavea.data <- anfavea.data[,1:2]
#ts trasnform
anfavea.data <- ts(anfavea.data[,2], freq = 1, start = 1957)
ts.plot(anfavea.data)

#subset anfavea data
anfavea.data.subset <- anfavea.data[39:62]
ts.plot(anfavea.data.subset)

#auto.arima ajusted model with lag 1 difference
f<- auto.arima(anfavea.data.subset);f

#Visual check of lag1 difference properties
#plot lag 1 diff
ts.plot(diff(anfavea.data.subset))
#mean line
abline(h = mean(diff(anfavea.data.subset)))
#Autocorrelation of lag 1 difference - looks good
acf((diff(anfavea.data.subset)))


#try 1
f <- Arima(anfavea.data.subset, order = c(1,1,0)) ; f
acf(f$residuals)
#try 2
f <- Arima(anfavea.data.subset, order = c(2,1,0)) ; f
acf(f$residuals) #looks good
#try 3
f <- Arima(anfavea.data.subset, order = c(3,1,0)) ; f
acf(f$residuals) #looks good
#try 4
f <- Arima(anfavea.data.subset, order = c(3,2,0)) ; f
acf(f$residuals) #

#select try 3 based on AIC, AICc and BIC
f <- Arima(anfavea.data.subset, order = c(3,1,0)) ; f
acf(f$residuals) #looks good

#check fi remains auto correlation
Box.test(f$residuals, lag=15, fitdf=4)
#Check residual distribuition
qqPlot(f$residuals) #hmm, not that good

#
plot(forecast(f,5))                   
lines(fitted(f), col=2) #terrible
