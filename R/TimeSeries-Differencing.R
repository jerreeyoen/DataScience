#author: Hanjie Yang
#Date: 11/20/2018
setwd("C:/Users/Jerry's surface/Documents/CompStatR")
#read data. car ownership in China
x = read.table("5.2.txt", header = TRUE)
car = x[,2]
ts.plot(car,type = "o")
y = diff(car)
plot(y,type = "o")
#The graph shows there is long-term trend on after 1st order differencing. Let's try a 2nd order difference.

y_1 = diff(car, differences = 2)
plot(y_1, type = "o")
#We can now see the PACF doesn't even close to 0 as the variance goes up as t goes up.


x1 = scan("5.3.txt")
plot(x1, type = "o")
x1_d1 = diff(x1)
plot(x1_d1, type = "o")
x1_d12 = diff(x1,12)
plot(x1_d12, type = "o")
## seasonal effect removed.


##shanghai Index, import financial data, first column is the data we want.
install.packages("fGarch")
library("fGarch")
data = read.table("book3.csv", header = F, sep = ",")
st1 = data[,1]
##use ts fucntion to convert and 52 weeks as frequency.
x = ts(st1, frequency = 52)
par(mfrow = c(3,2))
plot(x)
plot(diff(x))
acf(x)
acf(diff(x))
pacf(x)
pacf(diff(x))
## the original data shows strong clustering effect, acf is tailing off pacf shows 2 order cut off.
## the 1st order differecing still contains custering effect, after a ARIMA model we may need a GARCH model to solve inconstant variance.

#try ARIMA(1,1,1)
x.fit=arima(x, order = c (1,1,1))
#check with AIC
AIC(x.fit)
# check with SBC
AIC(x.fit, k = log(length(x)))

#try another model, AR(2)
x.fit2 = arima(x, order = c(2,0,0))
#check with AIC
AIC(x.fit2)
# check with SBC
AIC(x.fit2, k = log(length(x)))

#compare
AIC(x.fit) - AIC(x.fit2)
AIC(x.fit, k = log(length(x))) - AIC(x.fit2, k = log(length(x)))
# the number says the first one is better.
# keep in mind that the smaller the better.

#let R determine the parameters.
install.packages("forecast")
library(tseries)
library(forecast)
x.fit3 = auto.arima(x)
AIC(x.fit3)
AIC(x.fit)-AIC(x.fit3)
#the first model still works better, I think R didn't do a good job on Differencing, it assume the the variance is 0. Another guess will be the loop on variance is insufficent.

x.fit4 = auto.arima(diff(x))
AIC(x.fit) - AIC(x.fit4)

# if we do a first order differecing before using auto arima, it gives a best model.
library(fGarch)
#fit ARCH(1)

r.fit = garch(x.fit$residuals, order = c(0,1))
summary(r.fit)
AIC(r.fit)
#get volatility
vola= as.matrix(r.fit$fitted.values)
v1 = as.vector(vola[,1])
par(mfrow = c(1,1))
plot(v1, type = "l", main = "volatility of Shanghai Index")

# the volatility shows a pattern that match the original data points.
