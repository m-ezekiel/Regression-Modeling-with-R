#March 17, 2009

#Please change the file path in the command below to coincide with where you have stored the data files
setwd("C:/Users/sheather.ADSTAT/Documents/docs/AModernApproachToRegression/Data")

bridge <- read.table("bridge.txt", header=TRUE)
attach(bridge)

#Figure 7.1 on page 235
m1 <- lm(log(Time)~log(DArea)+log(CCost)+log(Dwgs)+log(Length)+log(Spans))
logDArea <- log(DArea)
logCCost <- log(CCost)
logDwgs <- log(Dwgs)
logLength <- log(Length)
logSpans <- log(Spans)
X <- cbind(logDArea,logCCost,logDwgs,logLength,logSpans)
install.packages("leaps")
library(leaps)
b <- regsubsets(as.matrix(X),log(Time))
rs <- summary(b)
par(mfrow=c(1,2))
plot(1:5,rs$adjr2,xlab="Subset Size",ylab="Adjusted R-squared")
library(car)
subsets(b,statistic=c("adjr2"))

#Table 7.1 on page 235
#Calculate adjusted R-squared
rs$adjr2
om1 <- lm(log(Time)~log(Dwgs))
om2 <- lm(log(Time)~log(Dwgs)+log(Spans))
om3 <- lm(log(Time)~log(Dwgs)+log(Spans)+log(CCost))
om4 <- lm(log(Time)~log(Dwgs)+log(Spans)+log(CCost)+log(DArea))
om5 <- m1
#Subset size=1
n <- length(om1$residuals)
npar <- length(om1$coefficients) +1
#Calculate AIC
extractAIC(om1,k=2)
#Calculate AICc
extractAIC(om1,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om1,k=log(n))
#Subset size=2
npar <- length(om2$coefficients) +1
#Calculate AIC
extractAIC(om2,k=2)
#Calculate AICc
extractAIC(om2,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om2,k=log(n))
#Subset size=3
npar <- length(om3$coefficients) +1
#Calculate AIC
extractAIC(om3,k=2)
#Calculate AICc
extractAIC(om3,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om3,k=log(n))
#Subset size=4
npar <- length(om4$coefficients) +1
#Calculate AIC
extractAIC(om4,k=2)
#Calculate AICc
extractAIC(om4,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om4,k=log(n))
#Subset size=5
npar <- length(om5$coefficients) +1
#Calculate AIC
extractAIC(om5,k=2)
#Calculate AICc
extractAIC(om5,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om5,k=log(n))

#Regression output on pages 235 and 236
summary(om2)
summary(om3)

#Output from R on page 237
backAIC <- step(m1,direction="backward", data=bridge)
backBIC <- step(m1,direction="backward", data=bridge, k=log(n))

#Output from R on page 238
mint <- lm(log(Time)~1,data=bridge)
forwardAIC <- step(mint,scope=list(lower=~1, 
upper=~log(DArea)+log(CCost)+log(Dwgs)+log(Length)+log(Spans)),
direction="forward", data=bridge)
forwardBIC <- step(mint,scope=list(lower=~1, 
upper=~log(DArea)+log(CCost)+log(Dwgs)+log(Length)+log(Spans)),
direction="forward", data=bridge,k=log(n))

detach(bridge)


prostateTraining <- read.table("prostateTraining.txt", header=TRUE)
attach(prostateTraining)

#Figure 7.2 on page 240
pairs(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45)

#Figure 7.3 on page 241
m1 <- lm(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45)
StanRes1 <- rstandard(m1)
par(mfrow=c(3,3))
plot(lcavol,StanRes1, ylab="Standardized Residuals")
plot(lweight,StanRes1, ylab="Standardized Residuals")
plot(age,StanRes1, ylab="Standardized Residuals")
plot(lbph,StanRes1, ylab="Standardized Residuals")
plot(svi,StanRes1, ylab="Standardized Residuals")
plot(lcp,StanRes1, ylab="Standardized Residuals")
plot(gleason,StanRes1, ylab="Standardized Residuals")
plot(pgg45,StanRes1, ylab="Standardized Residuals")
plot(m1$fitted.values,StanRes1, ylab="Standardized Residuals",xlab="Fitted values")

#Figure 7.4 on page 241
par(mfrow=c(1,1))
plot(m1$fitted.values,lpsa,xlab="Fitted Values")
abline(lsfit(m1$fitted.values,lpsa))

#Figure 7.5 on page 242
par(mfrow=c(2,2))
plot(m1)
abline(v=2*9/67,lty=2)

#Regression output on pages 242 and 243
summary(m1)

#Figure 7.6 page 243
library(alr3)
par(mfrow=c(3,3))
mmp(m1,lcavol,key=NULL)
mmp(m1,lweight,key=NULL)
mmp(m1,age,key=NULL)
mmp(m1,lbph,key=NULL)
mmp(m1,lcp,key=NULL)
mmp(m1,gleason,key=NULL)
mmp(m1,pgg45,key=NULL)
mmp(m1,m1$fitted.values,xlab="Fitted Values",key=NULL)

#R output on page 244
library(car)
vif(m1)

#Figure 7.7 on page 244
library(car)
par(mfrow=c(2,4))
avp(m1,variable=lcavol,ask=FALSE,identify.points=FALSE, main="")
avp(m1,variable=lweight,ask=FALSE,identify.points=TRUE, main="")
avp(m1,variable=age,ask=FALSE,identify.points=FALSE, main="")
avp(m1,variable=lbph,ask=FALSE,identify.points=FALSE, main="")
avp(m1,variable=svi,ask=FALSE,identify.points=FALSE, main="")
avp(m1,variable=lcp,ask=FALSE,identify.points=FALSE, main="")
avp(m1,variable=gleason,ask=FALSE,identify.points=FALSE, main="")
avp(m1,variable=pgg45,ask=FALSE,identify.points=FALSE, main="")

#Figure 7.8 on page 245
X <- cbind(lcavol,lweight,age,lbph,svi,lcp,gleason,pgg45)
library(leaps)
b <- regsubsets(as.matrix(X),lpsa)
rs <- summary(b)
par(mfrow=c(1,2))
library(car)
subsets(b,statistic=c("adjr2"),min.size=1,max.size=4,cex.subsets=0.7)
subsets(b,statistic=c("adjr2"),min.size=5,max.size=8,cex.subsets=0.7,legend=FALSE)

#Table 7.2 on page 245
#Calculate adjusted R-squared
rs$adjr2
om1 <- lm(lpsa~lcavol)
om2 <- lm(lpsa~lcavol+lweight)
om3 <- lm(lpsa~lcavol+lweight+svi)
om4 <- lm(lpsa~lcavol+lweight+svi+lbph)
om5 <- lm(lpsa~lcavol+lweight+svi+lbph+pgg45)
om6 <- lm(lpsa~lcavol+lweight+svi+lbph+pgg45+lcp)
om7 <- lm(lpsa~lcavol+lweight+svi+lbph+pgg45+lcp+age)
om8 <- m1
#Subset size=1
n <- length(om1$residuals)
npar <- length(om1$coefficients) +1
#Calculate AIC
extractAIC(om1,k=2)
#Calculate AICc
extractAIC(om1,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om1,k=log(n))
#Subset size=2
npar <- length(om2$coefficients) +1
#Calculate AIC
extractAIC(om2,k=2)
#Calculate AICc
extractAIC(om2,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om2,k=log(n))
#Subset size=3
npar <- length(om3$coefficients) +1
#Calculate AIC
extractAIC(om3,k=2)
#Calculate AICc
extractAIC(om3,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om3,k=log(n))
#Subset size=4
npar <- length(om4$coefficients) +1
#Calculate AIC
extractAIC(om4,k=2)
#Calculate AICc
extractAIC(om4,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om4,k=log(n))
#Subset size=5
npar <- length(om5$coefficients) +1
#Calculate AIC
extractAIC(om5,k=2)
#Calculate AICc
extractAIC(om5,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om5,k=log(n))
#Subset size=6
npar <- length(om6$coefficients) +1
#Calculate AIC
extractAIC(om6,k=2)
#Calculate AICc
extractAIC(om6,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om6,k=log(n))
#Subset size=7
npar <- length(om7$coefficients) +1
#Calculate AIC
extractAIC(om7,k=2)
#Calculate AICc
extractAIC(om7,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om7,k=log(n))
#Subset size=8
npar <- length(om8$coefficients) +1
#Calculate AIC
extractAIC(om8,k=2)
#Calculate AICc
extractAIC(om8,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om8,k=log(n))

#Regression output on page 246
summary(om2)
summary(om4)
summary(om7)

detach(prostateTraining)


prostateTest <- read.table("prostateTest.txt", header=TRUE)
attach(prostateTest)

#Regression output on page 247
om2 <- lm(lpsa~lcavol+lweight)
summary(om2)
om4 <- lm(lpsa~lcavol+lweight+svi+lbph)
summary(om4)
om7 <- lm(lpsa~lcavol+lweight+svi+lbph+pgg45+lcp+age)
summary(om7)

detach(prostateTest)


prostateTraining <- read.table("prostateTraining.txt", header=TRUE)
attach(prostateTraining)

#Figure 7.9 on page 249
X <- cbind(lcavol,lweight,age,lbph,svi,lcp,gleason,pgg45)
library(leaps)
b <- regsubsets(as.matrix(X),lpsa)
rs <- summary(b)
par(mfrow=c(1,2))
library(car)
subsets(b,statistic=c("adjr2"),main="With Case 45",min.size=1,max.size=5,cex.subsets=0.7)

m2 <- update(m1, subset=(1:67)[-c(45)])
lcavol1 <- lcavol[-c(45)]
lweight1 <- lweight[-c(45)]
age1 <- age[-c(45)]
lbph1 <- lbph[-c(45)]
svi1 <- svi[-c(45)]
lcp1 <- lcp[-c(45)]
gleason1 <- gleason[-c(45)]
pgg451 <- pgg45[-c(45)]
X <- cbind(lcavol1,lweight1,age1,lbph1,svi1,lcp1,gleason1,pgg451)
library(leaps)
b <- regsubsets(as.matrix(X),lpsa[-c(45)])
rs <- summary(b)
library(car)
subsets(b,statistic=c("adjr2"),main="Without Case 45",min.size=1,max.size=5,cex.subsets=0.7,legend=FALSE)

detach(prostateTraining) 


prostateAlldata <- read.table("prostateAlldata.txt", header=TRUE)
attach(prostateAlldata)

#Figure 7.10 on page 249
par(mfrow=c(1,1))
plot(lweight[train==FALSE],lpsa[train==FALSE],pch=2,col=1,xlab="lweight",ylab="lpsa",ylim=c(-1,6),xlim=c(2,6.5))
abline(lsfit(lweight[train==FALSE],lpsa[train==FALSE]),lty=1,col=1)
points(lweight[train==TRUE],lpsa[train==TRUE],pch=3,col=2)
abline(lsfit(lweight[train==TRUE],lpsa[train==TRUE]),lty=2,col=2)
legend(4.5,2,legend=c("Training","Test"),pch=3:2,col=2:1,title="Data Set")

detach(prostateAlldata)


prostateTest <- read.table("prostateTest.txt", header=TRUE)
attach(prostateTest)

#Figure 7.11 on page 250
m1 <- lm(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45)
library(car)
par(mfrow=c(1,1))
avp(m1,variable=lweight,ask=FALSE,identify.points=TRUE, main="")

detach(prostateTest)

#################EXERCISES

#Exercise 7.5.1

library(alr3)
data(mantel)
attach(mantel)

#Output from R: correlations on page 253
X <- cbind(X1,X2,X3)
cor(X)

#Figure 7.13 on page 254
library(leaps)
b <- regsubsets(as.matrix(X),Y)
rs <- summary(b)
par(mfrow=c(1,1))
library(car)
subsets(b,statistic=c("adjr2"),legend=FALSE)

#Table 7.4 on page 254
rs$adjr2
om1 <- lm(Y~X3)
om2 <- lm(Y~X1+X2)
om3 <- lm(Y~X1+X2+X3)
#Subset size=1
n <- length(om1$residuals)
npar <- length(om1$coefficients) +1
#Calculate AIC
extractAIC(om1,k=2)
#Calculate BIC
extractAIC(om1,k=log(n))
#Subset size=2
npar <- length(om2$coefficients) +1
#Calculate AIC
extractAIC(om2,k=2)
#Calculate BIC
extractAIC(om2,k=log(n))
#Subset size=3
npar <- length(om3$coefficients) +1
#Calculate AIC
extractAIC(om3,k=2)
#Calculate BIC
extractAIC(om3,k=log(n))

#Forward selection on pages 253 and 254
mint <- lm(Y~1,data=mantel)
forwardAIC <- step(mint,scope=list(lower=~1, 
upper=~X1+X2+X3),
direction="forward", data=mantel)
forwardBIC <- step(mint,scope=list(lower=~1, 
upper=~X1+X2+X3),
direction="forward", data=mantel,k=log(n))

#Regression output on page 255
summary(om1)
summary(om2)
summary(om3)

detach(mantel)


#Exercise 7.5.2
Hald <- read.table("Haldcement.txt",header=TRUE)
attach(Hald)

#Output from R: correlations on page 256
X <- cbind(x1,x2,x3,x4)
cor(X)

#Figure 7.14 on page 257
library(leaps)
b <- regsubsets(as.matrix(X),Y)
rs <- summary(b)
par(mfrow=c(1,1))
library(car)
subsets(b,statistic=c("adjr2"),legend=FALSE)

#Table 7.6 on page 257
rs$adjr2
om1 <- lm(Y~x4)
om2 <- lm(Y~x1+x2)
om3 <- lm(Y~x1+x2+x4)
om4 <- lm(Y~x1+x2+x3+x4)
#Subset size=1
n <- length(om1$residuals)
npar <- length(om1$coefficients) +1
#Calculate AIC
extractAIC(om1,k=2)
#Calculate AICc
extractAIC(om1,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om1,k=log(n))
#Subset size=2
npar <- length(om2$coefficients) +1
#Calculate AIC
extractAIC(om2,k=2)
#Calculate AICc
extractAIC(om2,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om2,k=log(n))
#Subset size=3
npar <- length(om3$coefficients) +1
#Calculate AIC
extractAIC(om3,k=2)
#Calculate AICc
extractAIC(om3,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om3,k=log(n))
#Subset size=4
npar <- length(om4$coefficients) +1
#Calculate AIC
extractAIC(om4,k=2)
#Calculate AICc
extractAIC(om4,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om4,k=log(n))

#Backward elimination based on AIC on page 257
backAIC <- step(om4,direction="backward", data=Hald)

#Backward elimination based on BIC on page 258
backBIC <- step(om4,direction="backward", data=Hald, k=log(n))

#Forward selection based on AIC on pages 258-259
mint <- lm(Y~1,data=Hald)
forwardAIC <- step(mint,scope=list(lower=~1, 
upper=~x1+x2+x3+x4),
direction="forward", data=Hald)

#Forward selection based on BIC on page 259
forwardBIC <- step(mint,scope=list(lower=~1, 
upper=~x1+x2+x3+x4),
direction="forward", data=Hald,k=log(n))

#Regression output from R on pages 259 and 260
summary(om1)
summary(om2)
library(car)
vif(om2)
summary(om3)
vif(om3)
summary(om4)
vif(om4)

detach(Hald)

