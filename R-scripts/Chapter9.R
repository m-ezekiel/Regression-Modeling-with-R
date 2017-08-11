#March 17, 2009

#Please change the file path in the command below to coincide with where you have stored the data files
setwd("C:/Users/sheather.ADSTAT/Documents/docs/AModernApproachToRegression/Data")

confood2 <- read.table("confood2.txt", header=TRUE)
attach(confood2)

#Figure 9.1 on page 306
par(mfrow=c(1,1))
plot(log(Price),log(Sales),xlab=expression(log(Price[t])),ylab=expression(log(Sales[t])),
pch=Promotion+2,col=Promotion+1)
legend(-0.3, 8.5,legend=c("No","Yes"),pch=2:3,col=1:2,title="Promotion")

#Figure 9.2 on page 307
plot(Week,log(Sales),type='o',ylab=expression(log(Sales[t])),xlab="Week, t",
pch=Promotion+2,col=Promotion+1)
legend(0, 8.5,legend=c("No","Yes"),pch=2:3,col=1:2,title="Promotion")

#Figure 9.3 on page 307
plot(log(SalesLag1),log(Sales),ylab=expression(log(Sales[t])),
xlab=expression(log(Sales[t-1])))

#Figure 9.4 on page 308
acf(log(Sales))

#Figure 9.5 on page 309
lsm1 <- lm(log(Sales)~log(Price)+Promotion+Week,data=confood2)
StanRes1 <- rstandard(lsm1)
par(mfrow=c(2,2))
plot(log(Price),StanRes1,ylab="Standardized Residuals",xlab=expression(log(Price[t])))
plot(Week,StanRes1,ylab="Standardized Residuals",xlab="Week, t",type='o')
plot(Promotion,StanRes1,ylab="Standardized Residuals")
plot(lsm1$fitted.values,StanRes1,ylab="Standardized Residuals",xlab="Fitted Values")

#Figure 9.6 on page 310
par(mfrow=c(1,1))
acf(StanRes1,main="Series Standardized Residuals")

#R output on page 313
install.packages("nlme")
library(nlme)
m1 <- gls(log(Sales)~log(Price)+Promotion+Week,correlation=corAR1(form=~Week),data=confood2,method="ML")
summary(m1)
intervals(m1)

#Figure 9.7 on page 314 
acf(m1$residuals,main="Series GLS Residuals")

#R output on page 318
g <- lm(log(Sales)~log(Price)+Promotion+Week,data=confood2)
rho <- 0.5504
x <- model.matrix(g)
Sigma <- diag(length(Week))
Sigma <- rho^abs(row(Sigma)-col(Sigma))
sm <- chol(Sigma)
smi <- solve(t(sm))
xstar <- smi %*% x
ystar <- smi %*% log(Sales)
m1tls <- lm(ystar ~ xstar-1) 
summary(m1tls)

#Figure 9.8 on page 318
par(mfrow=c(2,2))
plot(xstar[,1],ystar,xlab="Intercept*",ylab="log(Sales)*")
identify(xstar[,1],ystar,Week)
plot(xstar[,2],ystar,xlab="log(Price)*",ylab="log(Sales)*")
identify(xstar[,2],ystar,Week)
plot(xstar[,3],ystar,xlab="Promotion*",ylab="log(Sales)*")
identify(xstar[,3],ystar,Week)
plot(xstar[,4],ystar,xlab="Week*",ylab="log(Sales)*")
identify(xstar[,4],ystar,Week)

#Figure 9.9 on page 319
StanRes1 <- rstandard(m1tls)
par(mfrow=c(1,1))
acf(StanRes1,main="Series Standardized LS Residuals")

#Figure 9.10 on page 320
par(mfrow=c(2,2))
plot(xstar[,2],StanRes1,ylab="Standardized LS Residuals",xlab="log(Price)*")
plot(xstar[,4],StanRes1,ylab="Standardized LS Residuals",xlab="Week*",type='o')
identify(xstar[,4],StanRes1,labels=Week,cex=0.75)
plot(xstar[,3],StanRes1,ylab="Standardized LS Residuals",xlab="Promotion*")
plot(m1tls$fitted.values,StanRes1,ylab="Standardized LS Residuals",xlab="Fitted Values*")

#Figure 9.11 on page 320
par(mfrow=c(2,2))
plot(m1tls)
abline(v=2*4/length(Week),lty=2)

detach(confood2)


BayArea <- read.table("BayArea.txt",header=TRUE)
attach(BayArea)

#Figure 9.12 on page 321
pairs(InterestRate~LoansClosed+VacancyIndex)

#Figure 9.13 on page 322
m1 <- lm(InterestRate~LoansClosed+VacancyIndex)
summary(m1)
StanRes1 <- rstandard(m1)
mres1 <- lm(StanRes1~LoansClosed+I(LoansClosed^2))
a1 <- mres1$coeff[1]
a2 <- mres1$coeff[2]
a3 <- mres1$coeff[3]
mres2 <- lm(StanRes1~m1$fitted.values+I(m1$fitted.values^2))
b1 <- mres2$coeff[1]
b2 <- mres2$coeff[2]
b3 <- mres2$coeff[3]
par(mfrow=c(2,2))
plot(LoansClosed,StanRes1,ylab="Standardized Residuals")
curve(a1 + a2*x + a3*x^2, add = TRUE, col = "blue",lty=2)
plot(VacancyIndex,StanRes1,ylab="Standardized Residuals")
plot(m1$fitted.values,StanRes1,ylab="Standardized Residuals",xlab="Fitted Values")
curve(b1 + b2*x + b3*x^2, add = TRUE, col = "blue",lty=2)
acf(m1$residuals,main="Standardized LS Residuals")

#R output on page 323
library(nlme)
m1 <- gls(InterestRate~LoansClosed+VacancyIndex,correlation=corAR1(form=~Month),data=BayArea,method="ML")
acf(m1$residuals,main="GLS Residuals")
summary(m1)
intervals(m1)

#R output on page 323
g <- lm(InterestRate~LoansClosed+VacancyIndex,data=BayArea)
rho <- 0.9572093
x <- model.matrix(g)
Sigma <- diag(length(InterestRate))
Sigma <- rho^abs(row(Sigma)-col(Sigma))
sm <- chol(Sigma)
smi <- solve(t(sm))
xstar <- smi %*% x
ystar <- smi %*% InterestRate
m1tls <- lm(ystar ~ xstar-1) 
summary(m1tls)

#Figure 9.14 on page 324
par(mfrow=c(2,2))
plot(xstar[,1],ystar,xlab="Intercept*",ylab="InterestRate*")
identify(xstar[,1],ystar,Month)
plot(xstar[,2],ystar,xlab="LoansClosed*",ylab="InterestRate*")
identify(xstar[,2],ystar,Month)
plot(xstar[,3],ystar,xlab="VacancyIndex*",ylab="InterestRate*")
identify(xstar[,3],ystar,Month)
plot(xstar[,2],xstar[,3],xlab="LoansClosed*",ylab="VacancyIndex*")
identify(xstar[,2],xstar[,3],Month)


#Figure 9.15 on page 325
StanRes1 <- rstandard(m1tls)
par(mfrow=c(2,2))
acf(StanRes1,main="Standardized LSResiduals")
plot(xstar[,2],StanRes1,ylab="Standardized LS Residuals",xlab="LoansClosed*")
identify(xstar[,2],StanRes1,Month)
plot(xstar[,3],StanRes1,ylab="Standardized LS Residuals",xlab="VacancyIndex*")
identify(xstar[,3],StanRes1,Month)
plot(m1tls$fitted.values,StanRes1,ylab="Standardized Residuals",xlab="Fitted Values*")
identify(m1tls$fitted.values,StanRes1,Month)

detach(BayArea)



#################EXERCISES

#Exercise 9.4.1

boxoffice <- read.table("boxoffice.txt", header=TRUE)
attach(boxoffice)

#Figure 9.16 on page 326
YearsS1975 <- year - 1975
lsm1 <- lm(GrossBoxOffice~YearsS1975,data=boxoffice)
StanRes1 <- rstandard(lsm1)
par(mfrow=c(2,2))
plot(YearsS1975,GrossBoxOffice,ylab="Gross Box Office ($M)",xlab="Years since 1975")
abline(lsm1,lty=2)
plot(YearsS1975,StanRes1,ylab="Standardized Residuals",xlab="Years since 1975")
acf(StanRes1,main="Series Standardized Residuals")

#R output on page 327
library(nlme)
m1 <- gls(GrossBoxOffice~YearsS1975,correlation=corAR1(form=~YearsS1975),data=boxoffice,method="ML")
summary(m1)

#R output on page 327
g <- lm(GrossBoxOffice~YearsS1975,data=boxoffice)
rho <- 0.8782065
x <- model.matrix(g)
Sigma <- diag(length(YearsS1975))
Sigma <- rho^abs(row(Sigma)-col(Sigma))
sm <- chol(Sigma)
smi <- solve(t(sm))
xstar <- smi %*% x
ystar <- smi %*% GrossBoxOffice
m1tls <- lm(ystar ~ xstar-1) 
summary(m1tls)

#Figure 9.17 on page 328
StanRes1 <- rstandard(m1tls)
mres2 <- lm(StanRes1~m1tls$fitted.values+I(m1tls$fitted.values^2)+I(m1tls$fitted.values^3))
b1 <- mres2$coeff[1]
b2 <- mres2$coeff[2]
b3 <- mres2$coeff[3]
b4 <- mres2$coeff[4]
mres3 <- lm(StanRes1~m1tls$fitted.values+I(m1tls$fitted.values^2)+I(m1tls$fitted.values^3)+I(m1tls$fitted.values^4)+I(m1tls$fitted.values^5))
par(mfrow=c(1,2))
plot(m1tls$fitted.values,StanRes1,ylab="Standardized LS Residuals",xlab="Fitted Values*")
curve(b1 + b2*x + b3*x^2 + + b4*x^3, add = TRUE,lty=2)
acf(StanRes1,main="Stand LS Residuals")

detach(boxoffice)

