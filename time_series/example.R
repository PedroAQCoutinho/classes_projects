###  Café
d <- read.csv("data/producaocafe.csv",sep=",")
names(d)
ts.plot(ts(d[,2],freq=1, start=1900))

y<- ts(d[61:121,2], freq=1, start=1960)
plot(y, ylab="Produção de café (t)")

(f<- auto.arima(y))
acf(y)

(f<- Arima(y, order=c(5,0,0)))  # AR(5)

acf(f$residuals)
Box.test(f$residuals, lag=15, fitdf=5)   # sobrou autocorrelação
library(car)
qqPlot(f$residuals)

(f2<- Arima(y, order=c(5,0,0), fixed=c(0,NA,0,0,NA,NA)))# zero phi1, phi3, phi4
names(f2)
(Q<--2*(f2$loglik-f$loglik))    # teste de razão de verossimilhanças
1-pchisq(Q,3)

plot(y, ylab="Produção de café (t)")   # parece ok
lines(fitted(f2), col=2)

plot(forecast(f2,5))                   # ih, que horror

y2<- d[104:121,2]                      # série a partir de 2003
ts.plot(y2)
tend<- 0:17
reg<- lm(y2 ~ tend)                    # tendência linear
acf(reg$residuals)

(regar <- Arima(y2, order=c(2,0,0), xreg=tend)) # reg com erro AR(2)
acf(regar$residuals)
ts.plot(y2)
lines(fitted(regar), col=2)

plot(forecast(regar,3,xreg=c(19,20,21)))
plot(forecast(regar,10,xreg=(19:28)))

auto.arima(y2)
(far<- Arima(y2, order=c(1,1,0), include.constant=TRUE) ) 
ts.plot(y2)
lines(fitted(far), col=4)
lines(fitted(regar), col=2)

(far<- Arima(diff(y2), order=c(1,0,0), include.constant=TRUE) )

