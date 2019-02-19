#Author:hanjie Yang
#Date:3/28/2017

#found this on my previous HW. it's about smoothing.

S<-log(c(25, 42, 45, 46, 51, 103, 124,146,340,396,412,876,1112))
B<-log(c(24,40,719,727,791,1166,1235,1581,1804,3460,3808))
#set B =1000
T<-1000
#store jth info for Stomach
BootStrapS<-NULL
MeanS<-NULL
MeanB<-NULL
for (i in 1:T){
  temp<-sample(S,replace=TRUE)
  BootStrapS=c(BootStrapS,var(temp))
  MeanS=c(MeanS,mean(temp))
}
#jth info for Breast
BootStrapB<-NULL
for (i in 1:T){
  temp<-sample(B,replace=TRUE)
  BootStrapB=c(BootStrapB,var(temp))
  MeanB=c(MeanB,mean(temp))
}
#variance and mean for bootstrap Breast and Stomach 
varB<-mean(BootStrapB)
varS<-mean(BootStrapS)
means<-mean(MeanS)
meanb<-mean(MeanB)
#variance inference
realvarB<-sum((BootStrapB-varB)^2)/(T-1)
realvarS<-sum((BootStrapS-varS)^2)/(T-1)



## student-t
TB<-quantile(BootStrapB,probs = c(0.025,0.975))
TS<-quantile(BootStrapS,probs = c(0.025,0.975))

#95% CI for Stomach
StomachResult<-c(means-sqrt(realvarB)*TS[1],means+sqrt(realvarB)*TS[2])
#95% CI for Breast
BreastResult<-c(meanb-sqrt(realvarS)*TB[1], meanb+sqrt(realvarS*TS[2]))


#PartB
mean0B<-mean(B)
mean0S<-mean(S)
t0<-abs(mean0B-mean0S)
#newsamples
ti<-NULL
for (i in 1:10000){
newb<-sample(c(B,S),11)
news<-setdiff(c(B,S),newb)
ti=c(ti,abs(mean(newb)-mean(news)))
}
ti=sort(ti)
for (i in 1:10000){
  if (ti[i]>t0){
    print(i-1)
    break
  }
}

#part C
CS<-exp(S)
CB<-exp(B)
#Bootstrap for Stomach
CBS=(replicate(T,sample(CS,replace=TRUE)))
CBSmean<-apply(CBS,2,mean)
CBSCI<-quantile(CBSmean,probs = c(0.025,0.975))
#Bootstrap for Breast
CBB=(replicate(T,sample(CB,replace=TRUE)))
CBBmean<-apply(CBB,2,mean)
CBBCI<-quantile(CBBmean,probs = c(0.025,0.975))

StomachFromA<-exp(StomachResult)
BreastFromA<-exp(BreastResult)
print(StomachFromA)
print(CBSCI)
print(BreastFromA)
print(CBBCI)

#Question 2
#Part A
#set n for sample size
n<-c(10,100,1000,10000)
par(mfrow=c(4,1))
for (i in 1:4){
  EsampleQ2a<-rcauchy(n[i])
  BsampleQ2a<-replicate(1000,sample(EsampleQ2a,replace=TRUE))
  BsampleQ2aBias<-2*mean(EsampleQ2a)-mean(apply(BsampleQ2a,2,mean))
  hist(BsampleQ2a,main = paste0("cauchy size = ",n[i]),xlab=paste0("bias = ", BsampleQ2aBias, "     mean = ",mean(apply(BsampleQ2a,2,mean))))
  abline(v=mean(apply(BsampleQ2a,2,mean)),col="red")
  
}


#partB

par(mfrow=c(1,1))

n<-1000
M<-10
  EsampleQ2b<-runif(n)*M
  BsampleQ2b<-replicate(10000,runif(n,0,max(EsampleQ2b)))

  hist(apply(BsampleQ2b,2,max),breaks = 40,main = paste0("sample size = ", n, " max = ", M), xlab = "X")
 #  abline(v=mean(apply(BsampleQ2b,2,mean)),col="red")

  



#Question 4
Q4random<-rnorm(100)
Mu<-mean(Q4random)
#B
t<-10
bof4<-(replicate(t,sample(Q4random,replace=TRUE)))
#mean
bof4mean<-apply(bof4,2,mean)
#bias
bof4bias<-2*Mu-mean(bof4mean)
#variance
bof4var<-sum((mean(var(bof4))-var(bof4))^2)/(t-1)
#C
cof4<-NULL
for(i in 1:t){
  cof4<-c(cof4,Q4random)
}
cof4per<-matrix(sample(cof4), nrow=100)

#mean
cof4mean<-mean(cof4per)
#bias
cof4bias<-2*Mu-mean(cof4mean)
#Variance
cof4var<-sum((mean(var(cof4per))-var(cof4per))^2)/(t-1)

print(bof4var)
print(cof4var)
print(Mu)
print(bof4bias)
print(cof4bias)
print(mean(bof4mean))
