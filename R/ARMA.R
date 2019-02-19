#Author: Hanjie Yang
#Date: 10/14/2017

setwd("C:/Users/Jerry's surface/Documentss/TimeSeries/a")
library(fBasics)

#simulate a MA(2) with std = 1.
set.seed(2018)
y.ma = arima.sim(n = 10000, list(ma = c(0.5,0.2)), sd = 1)

fit.ma <- arima(y.ma, order = c(0,0,2), include.mean = F)
ypred.ma = predict(fit, n.ahead = 6)

summary(fit.ma)
fit.ma
#show prediction and errors, after 2 preiod should converge to 0.
ypred.ma$pred
ypred.ma$se

#simulate an AR(2)

set.seed(2018)
y.ar = arima.sim(n = 10000, list(ar = c(0.5,0.2)), sd = 1)

fit.ar <- arima(y.ar, order = c(2,0,0), include.mean = F)
ypred.ar = predict(fit.ar, n.ahead = 6)

summary(fit.ar)
fit.ar

#show prediction and errors
ypred.ar

#now, try arma(2,2)
y.arma = arima.sim(n = 10000, list(ar = c(0.5,0.2), ma = c(0.2,0.3)), sd = 1)
fit.arma <- arima(y.arma, order = c(2,0,2), include.mean = F)
ypred.arma = predict(fit.arma, n.ahead = 6)

summary(fit.arma)
fit.arma
ypred.arma


#compare acf and PACF of AR and MA models.
par(mfrow = c(2,2))
acf(y.ma,  main = "ACF of MA(2)")
acf(y.ar,  main = "ACF of AR(2)")
pacf(y.ma, main = "PACF of MA(2)")
pacf(y.ma, main = "PACF of AR(2)")
#The method used by the package is method of moment by Yule_walker.
#the charactors of AR are obvious, the ACF is tail off and the PACF is cut off at lag 2.
#the charactors of MA are obvious as well, the ACF is cut off at lag 2 and the PACF is tail off.

#see the ACF and PACF of ARMA
par(mfrow = c(2,1))
acf(y.arma, main = " ACF of ARMA(2,2)")
pacf(y.arma, main = "PACF of ARMA(2,2)")
#ACF tail off and PACF cut off.
fit.arma
tsdiag(fit.arma)
#we can't reject the null hypothesis that the residuals are white noise. Thus, I say the model is ideal.


#working with real world data.
x= read.csv("ex3_13.csv", header = F)
xx=x[,1]
par(mfrow=c(1,1))
plot(xx,type = "o")
#it appears no seasonal and trending effect.

library(TSA)
#check if it's a white noise dataset.
#write a function to do box test for lag from 1 to 12 using 2 different method.

box_test=function(x,n){
  Q_sta  = NULL
  LB_sta = NULL
  Q_pv   = NULL
  LB_pv  = NULL
  for(i in 1:n){
    test1     = Box.test(x, lag = i, type = "Box")
    test2     = Box.test(x, lag = i, type = "Ljung")
    Q_sta[i]  = test1$statistic
    Q_pv[i]   = test1$p.value
    LB_sta[i] = test2$statistic
    LB_pv[i]  = test2$p.value
  }
  result = data.frame(Q_sta, Q_pv, LB_sta,LB_pv)
  return(result)
}
t_ma = box_test(xx,12)
# p values are less than 0.05 so we assume they are not white noise.
acf(xx)
fit.ma.real = arima(xx,order = c(0,0,2))
fit.ma.real
acf(fit.ma.real$residuals)
#the residual is white noise now, comfirm with LB test
Box.test(fit.ma.real$residuals, lag = 24, type = "Ljung")
#P value > 0.05 as 0.9539

xx.pred = predict(fit.ma.real, n.ahead = 6)
plot(xx, type = "l", xlim = c(0,80), ylim = c(20,80))
lines(xx.pred$pred,col = "red")
#show a 95% CI area
lines(xx.pred$pred+1.96*xx.pred$se, col = "blue", lty = 5)
lines(xx.pred$pred-1.96*xx.pred$se, col = "blue", lty = 5)


#AR model
pacf(xx)
#cut off patter at lag 1, it might fit an AR model.
fit.ar.order = ar(xx, method = "mle")
#find best AR model using maximum likelihood mothod under mimimum AIC priciple
fit.ar.real = arima(xx,order = c(fit.ar.order$order,0,0))
fit.ar.real
#do a white noise test on residual
pacf(fit.ar.real$residuals)
Box.test(residuals(fit.ar.real), lag = 6, type ="Ljung")
#p value of .931 is greater than 0.05, so the residuals are white noise

