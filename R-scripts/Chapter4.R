#March 17, 2009

#Please change the file path in the command below to coincide with where you have stored the data files
setwd("C:/Users/sheather.ADSTAT/Documents/docs/AModernApproachToRegression/Data")

cleaningwtd <- read.table("cleaningwtd.txt",header=TRUE)
attach(cleaningwtd)

#Regression output on page 117
wm1 <- lm(Rooms~Crews,weights=1/StdDev^2)
summary(wm1)
predict(wm1,newdata=data.frame(Crews=c(4,16)),interval="prediction",level=0.95)

#Regression output on page 120
ynew <- Rooms/StdDev
x1new <- 1/StdDev
x2new <- Crews/StdDev
wm1check <- lm(ynew~x1new + x2new - 1)
summary(wm1check)
predict(wm1check,newdata=data.frame(x1new=c(1/4.966555,1/12.000463),x2new=c(4/4.966555,16/12.000463)),interval="prediction",level=0.95)

detach(cleaningwtd)

#################EXERCISES

#Ex 4.2.3

Houston <- read.table("HoustonRealEstate.txt",header=TRUE)
attach(Houston)

#Figure 4.1 on page 123
m1 <- lm(Yi~x1i+x2i,weights=ni)
leverage1 <- hatvalues(m1)
StanRes1 <- rstandard(m1)
absrtsr1 <- sqrt(abs(StanRes1))
residual1 <- m1$residuals
par(mfrow=c(2,3))
plot(x1i,Yi)
plot(x2i,Yi)
plot(x1i,x2i)
plot(m1$fitted.values,StanRes1, ylab="Standardized Residuals",xlab="Fitted Values")
abline(h=2,lty=2)
abline(h=-2,lty=2)
plot(m1$fitted.values,absrtsr1,ylab="Square Root(|Standardized Residuals|)",xlab="Fitted Values")
abline(lsfit(m1$fitted.values,absrtsr1),lty=2,col=2)

detach(Houston)