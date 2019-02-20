setwd("C:/Users/jerre/Documents/Rcode")
consume = scan("4.1.txt")
ts.plot(consume, type = "o")
t = 1:39
#fit a model
model = lm(consume ~ t)
#print the statistic of the model and test the model.
summary(model)

#add a line on the plot
abline(model, col = "red")

#2nd simulation
index = scan("4.2.txt")
t2 = 1:130
t2_2 = t2^2
model_2 = lm(index~t2+t2_2)
summary(model_2)
#the t2 has pvalue = 0.45, so i took it off.
model_2_1 = lm(index~t2_2)
summary(model_2_1)
#show the graph
ts.plot(index)
lines(fitted(model_2_1), col="blue")

#add one more features as t^3
t2_3 = t2^3
model_2_2 =lm(index~t2+t2_2 +t2_3)
summary(model_2_2)

# it appears it's the best so far. 
lines(fitted(model_2_2), col="red")

x <- read.table("4.6.txt", header = T)
temp = c(x[,1],x[,2],x[,3],x[,4],x[,5],x[,6])
plot(temp, type="o")
#there is seasonal effect but no trending

#getting seasonal index
temp.seasonal = NULL
for (i in 1:12){
  sum_i =0
  for(j in 1:6){
    sum_i = sum_i + temp[i+12*(j-1)]
  }
  temp.seasonal[i]=(sum_i/6)/mean(temp)
  
}
temp.seasonal
plot(temp.seasonal,type = "o")
# it appears a pattern that the index in increasing before july and decresing after july
# using R to convert to timeseries
temp.ts = ts(temp, frequency = 12)
# just spoiled by R, using decompose to show the indexes
plot(decompose(temp.ts))

#complete time series analysis process
x=read.table("4.7.txt", header = T)
sale = c(x[,1],x[,2],x[,3],x[,4],x[,5],x[,6],x[,7],x[,8])
plot(sale, type = "o")
sale.seasonal= NULL
for (i in 1:12){
  sum_i =0
  for(j in 1:8){
    sum_i = sum_i + sale[i+12*(j-1)]
  }
  sale.seasonal[i]=(sum_i/8)/mean(sale)
  
}
sale.seasonal
plot(sale.seasonal, type = "l")
#remove seasonal effect

z =rep(sale.seasonal,8)
sale.deseasonal = sale/z
ts.plot(sale.deseasonal, type = "o")
t = 1:(12*8)
sale.lm = lm(sale.deseasonal~t)
lines(fitted(sale.lm),col = "red")
summary(sale.lm)

#prediction
sale.predict = NULL
for(i in 97:108){
  sale.predict[i]=1015.5222+20.9318*i
}
# multiply by seasonal effect
sale.pres = sale.predict*sale.seasonal
ts.plot(sale,xlim=c(0,120), ylim=c(0,5000), type = "o")
lines(sale.pres, col="red")
abline(v=97, col="black")
#the predition on the graph looks reasonable.
#using decompse
sales.ts=ts(sale,frequency = 12)
plot(decompose(sales.ts))
#comfirmed that seasonal effect and trending are similar with my models.
