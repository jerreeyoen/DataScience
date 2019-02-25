##Author: hanjie Yang
##Date: 2/25/2019
## this experience is to simulate 2 independent random walk series.
## Then I am testing if they have linear relationship with 95% CI
## We know that they should have no relationship and only about 5 % of data will say that they are related.
## here is the simulation.
AlphaTest <- function(n){
  count = 0
for (i in 1:100){
##simulate random walk 1
  x <- w <- rnorm(1000)
for (t in 2:1000) x[t] <- x[t - 1] + w[t]
##simulate random walk 2
y <-w <- rnorm(1000)
for (t in 2:1000) x[t] <- x[t - 1] + w[t]
##do a corelation t-test
temp <- cor.test(x, y, method=c("pearson", "kendall", "spearman"))
## if pvalue is less than alpha, 0.05, then we should reject null hypothesis.
if (temp$p.value < 0.05)
  #see how many times we reject null. ideally around 5.
  count = count + 1
}
  print(count)
}

AlphaTest(100)

##My result says there is 27 times that we will reject the null hypothesis.
##it's surprise that the model gives significant conclustion for 27 times when I use 5% alpha.

