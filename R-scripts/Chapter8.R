#March 17, 2009

#Please change the file path in the command below to coincide with where you have stored the data files
setwd("C:/Users/sheather.ADSTAT/Documents/docs/AModernApproachToRegression/Data")

MichelinFood <- read.table("MichelinFood.txt", header=TRUE)
attach(MichelinFood)

#Figure 8.1 on page 266
plot(Food,proportion,ylab="Sample proportion",xlab="Zagat Food Rating")

#R output on page 267
m1 <- glm(cbind(InMichelin,NotInMichelin)~Food,family=binomial)
summary(m1)

#Figure 8.2 on page 268
x <- seq(15,28,0.05)
y <- 1/(1+exp(-1*(m1$coeff[1] + m1$coeff[2]*x)))
plot(Food,proportion,ylab="Probability of inclusion in the Michelin Guide",xlab="Zagat Food Rating")
lines(x,y)

#Table 8.2 on page 269
thetahat <- m1$fitted.values
odds_ratio <- m1$fitted.values/(1-m1$fitted.values)
cbind(Food,round(thetahat,3),round(odds_ratio,3))

#p-value on page 272
pchisq(m1$deviance,m1$df.residual,lower=FALSE)

#Value of the difference in devinace and associated p-value on page 273
m1$null.deviance-m1$deviance
pchisq(m1$null.deviance-m1$deviance,1,lower=FALSE)

#Logistic regression output on page 274
print(paste("Pearson's X^2 =",round(sum(residuals(m1,type="pearson")^2),3)))

#Table 8.3 on page 276
cbind(round(residuals(m1,"response"),3),round(residuals(m1,"pearson"),3),round(residuals(m1,"deviance"),3))

#Figure 8.3 on page 276
hvalues <- influence(m1)$hat
stanresDeviance <- residuals(m1)/sqrt(1-hvalues)
stanresPearson <- residuals(m1,"pearson")/sqrt(1-hvalues)
par(mfrow=c(1,2))
plot(Food,stanresDeviance,ylab="Standardized Deviance Residuals",xlab="Food Rating",ylim=c(-2,2))
plot(Food,stanresPearson,ylab="Standardized Pearson Residuals",xlab="Food Rating",ylim=c(-2,2))

detach(MichelinFood)


MichelinNY <- read.csv("MichelinNY.csv", header=TRUE)
attach(MichelinNY)

y <- InMichelin

#Figure 8.4 on page 278
par(mfrow=c(1,1))
plot(jitter(Food,amount=.15),jitter(y,amount=0.03),xlab="Food Rating",
ylab="In Michelin Guide? (0=No, 1=Yes)")

#Figure 8.5 on page 279
boxplot(Food~y, ylab="Food Rating",xlab="In Michelin Guide? (0=No, 1=Yes)")

#Logistic regression output on page 279
m1 <- glm(y~Food,family=binomial(),data=MichelinNY)
summary(m1)

#Figure 8.6 on page 281
hvalues <- influence(m1)$hat
stanresDeviance <- residuals(m1)/sqrt(1-hvalues)
#Alternatively we could use 
#stanresDeviance < rstandard(m1)
stanresPearson <- residuals(m1,"pearson")/sqrt(1-hvalues)
par(mfrow=c(1,2))
plot(Food,stanresDeviance,ylab="Standardized Deviance Residuals",xlab="Food Rating",ylim=c(-4.1,4.1))
plot(Food,stanresPearson,ylab="Standardized Pearson Residuals",xlab="Food Rating",ylim=c(-4.1,4.1))

#Figure 8.7 on page 282
par(mfrow=c(1,1))
xx <- seq(15,28.2,0.05)
yy <- 1/(1+exp(-1*(m1$coeff[1] + m1$coeff[2]*xx)))
loessfit1 <- loess(y ~ Food,degree=1,span=2/3)
plot(jitter(Food,amount=.15),jitter(y,amount=0.03),xlab="Food Rating",
ylab="In Michelin Guide? (0=No, 1=Yes)")
lines(xx,yy)
lines(xx,predict(loessfit1,data.frame(Food=xx)),lty=2)

#Figure 8.8 on page 286
par(mfrow=c(2,2))
boxplot(Food~y, ylab="Food Rating",xlab="In Michelin Guide? (0=No, 1=Yes)")
boxplot(Decor~y, ylab="Decor Rating",xlab="In Michelin Guide? (0=No, 1=Yes)")
boxplot(Service~y, ylab="Service Rating",xlab="In Michelin Guide? (0=No, 1=Yes)")
boxplot(Price~y, ylab="Price",xlab="In Michelin Guide? (0=No, 1=Yes)")

#Figure 8.9 on page 288
m2 <- glm(y~Food+Decor+Service+Price+log(Price),family=binomial(),data=MichelinNY)
loessfit1 <- loess(y ~ Food,degree=1,span=2/3)
loessfit2 <- loess(m2$fitted.values ~ Food,degree=1,span=2/3)
xx <- seq(15,28.2,0.05)
summary(m2)
par(mfrow=c(1,2))
plot(Food,y,xlab="Food Rating, x1", ylab="Y, In Michelin Guide? (0=No, 1=Yes)")
lines(xx,predict(loessfit1,data.frame(Food=xx)))
#lines(lowess(Food,y,iter=1,f=2/3))
plot(Food,m2$fitted.values,ylab=expression(hat(Y)),xlab="Food Rating, x1")
lines(xx,predict(loessfit2,data.frame(Food=xx)))

#Figure 8.10 on page 288
library(alr3)
mmps(m2,layout=c(2,3),key=NULL)

#Figure 8.11 on page 289
par(mfrow=c(1,1))
plot(Decor,Service,pch=y+1,col=y+1,xlab="Decor Rating",ylab="Service Rating")
abline(lsfit(Decor[y==0],Service[y==0]),lty=1,col=1)
abline(lsfit(Decor[y==1],Service[y==1]),lty=2,col=2)
legend(14, 28,legend=c("No","Yes"),pch=1:2,col=1:2,title="In Michelin Guide?")

#Figure 8.12 on page 290
m3 <- glm(y~Food+Decor+Service+Price+log(Price)+Service:Decor,family=binomial(),data=MichelinNY)
mmps(m3,layout=c(2,3),key=NULL)

#Output from R on page 290
anova(m2,m3,test="Chisq")

#Figure 8.13 on page 291
par(mfrow=c(1,1))
hvalues <- influence(m3)$hat
stanresDeviance <- residuals(m3)/sqrt(1-hvalues)
plot(hvalues,stanresDeviance,ylab="Standardized Deviance Residuals",xlab="Leverage Values",ylim=c(-3,3),xlim=c(-0.05,0.7))
abline(v=2*7/length(y),lty=2)
identify(hvalues,stanresDeviance,labels=Restaurant.Name,cex=0.75)

#Output from R on pages 291 and 292
summary(m3)

#Output from R on pages 292 and 293
m4 <- glm(y~Food+Decor+Service+log(Price)+Service:Decor,family=binomial(),data=MichelinNY)
anova(m4,m3,test="Chisq")
summary(m4)

#Figure 8.14 on page 294
mmps(m4,layout=c(2,3),key=NULL)

#Figure 8.15 on page 295
par(mfrow=c(1,1))
hvalues <- influence(m4)$hat
stanresDeviance <- residuals(m4)/sqrt(1-hvalues)
plot(hvalues,stanresDeviance,ylab="Standardized Deviance Residuals",xlab="Leverage Values",ylim=c(-3,3),xlim=c(-0.05,0.35))
abline(v=2*6/length(y),lty=2)
identify(hvalues,stanresDeviance,labels=Restaurant.Name,cex=0.75)

#Table 8.5 on page 295
fits4 <- m4$fitted.values
round(fits4[c(14,37,69,133,135,138,160)],3)

detach(MichelinNY)


#################EXERCISES

#Exercise 8.3.1

playoffs <- read.table("playoffs.txt",header=TRUE)
attach(playoffs)

#Figure 8.16 on page 296
plot(Population,PlayoffAppearances,xlab="x, Population (in millions)", 
ylab="Y, Play off Appearances (in 10 seasons)")

#Output from R on page 296
m1 <- lm(PlayoffAppearances~Population)
summary(m1)

detach(playoffs)


#Exercise 8.3.3

ex833 <- read.csv("HeartDisease.csv", header=TRUE)
attach(ex833)

#Output from R for model (8.6) on page 299
m1 <- glm(HeartDisease~x1 + x2 + x3 + x4 + x5,
family=binomial(),data=ex833)
summary(m1)
 
#Figure 8.17 on page 298
library(alr3)
par(mfrow=c(3,2))
mmp(m1,x1,key=NULL)
mmp(m1,x2,key=NULL)
mmp(m1,x4,key=NULL)
mmp(m1,x5,key=NULL)
mmp(m1,m1$fitted.values,xlab="Fitted Values",key=NULL)

#Figure 8.18 on page 299
y <- HeartDisease
par(mfrow=c(2,1))
plot(density(x1[y==0],bw="SJ",kern="gaussian"),type="l",
main="Gaussian kernel density estimate",xlab="x1")
rug(x1[y==0])
lines(density(x1[y==1],bw="SJ",kern="gaussian"),lty=2)
rug(x1[y==1])
legend(190, 0.0275,legend=c("No","Yes"),lty=1:2,title="Heart Disease?")
plot(density(x4[y==0],bw="SJ",kern="gaussian"),type="l",ylim=c(0,0.1),
main="Gaussian kernel density estimate",xlab="x4")
rug(x4[y==0])
lines(density(x4[y==1],bw="SJ",kern="gaussian"),lty=2)
rug(x4[y==1])
legend(40.5, 0.1,legend=c("No","Yes"),lty=1:2,title="Heart Disease?")

#Output from R for model (8.7) on page 300
f1x1 <- log(x1)
f2x4 <- log(x4)
m2 <- glm(HeartDisease~x1 + f1x1 + x2 + x3 + x4 + f2x4 + x5,
family=binomial(),data=ex833)
summary(m2)

#Figure 8.19 on page 300
par(mfrow=c(3,3))
mmp(m2,x1,key=NULL)
mmp(m2,f1x1,key=NULL)
mmp(m2,x2,key=NULL)
mmp(m2,x4,key=NULL)
mmp(m2,f2x4,key=NULL)
mmp(m2,x5,key=NULL)
mmp(m2,m2$fitted.values,xlab="Fitted Values",key=NULL)

detach(ex833)


#Exercise 8.3.6
library(alr3)
data(banknote)
attach(banknote)

#Figure 8.20 on page 303
par(mfrow=c(1,1))
plot(Diagonal,Bottom,pch=Y+1,col=Y+1)
legend(141, 12.5,legend=c("Yes","No"),pch=1:2,col=1:2,title="Counterfeit?")
detach(banknote)

