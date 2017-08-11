#April 21, 2009

#Please change the file path in the command below to coincide with where you have stored the data files
setwd("C:/Users/sheather.ADSTAT/Documents/docs/AModernApproachToRegression/Data")

bimodal <- read.table("bimodal.txt", header=TRUE)
attach(bimodal)
x <- bimodal$x
n<-length(x)
xx <- c(-300:300)/100

#Figure A.1 on page 372
h <- 0.25
plot( x=c(-3,3),y=c(0,0.65),type="n",xlab="x",ylab="Estimated & True Densities")
ysum <- numeric(601)
for (i in 1:n)
{points(x[i], 1/(n*h*sqrt(2*pi)),type="h")
x1 <- numeric(601)+x[i]
y <- (1/(h*sqrt(2*pi)))*exp(-0.5*((xx-x1)/h)^2)
ysum <- y/n + ysum
lines(xx,y/n,lty=1)}
lines(xx,ysum,lty=1)
truedensity <- 0.5*(3/(sqrt(2*pi)))*exp(-0.5*((xx+1)/(1/3))^2) + 0.5*(3/(sqrt(2*pi)))*exp(-0.5*((xx-1)/(1/3))^2)
lines(xx,truedensity,lty=2)

#Figure A.2 on page 373
h <- 0.6
plot( x=c(-3,3),y=c(0,0.65),type="n",xlab="x",ylab="Estimated & True Densities")
ysum <- numeric(601)
for (i in 1:n)
{points(x[i], 1/(n*h*sqrt(2*pi)),type="h")
x1 <- numeric(601)+x[i]
y <- (1/(h*sqrt(2*pi)))*exp(-0.5*((xx-x1)/h)^2)
ysum <- y/n + ysum
lines(xx,y/n,lty=1)}
lines(xx,ysum,lty=1)
truedensity <- 0.5*(3/(sqrt(2*pi)))*exp(-0.5*((xx+1)/(1/3))^2) + 0.5*(3/(sqrt(2*pi)))*exp(-0.5*((xx-1)/(1/3))^2)
lines(xx,truedensity,lty=2)

#A.2
install.packages("KernSmooth")
library(KernSmooth)

curve <- read.table("curve.txt", header=TRUE)
attach(curve)

x <- curve$x
y <- curve$y
n <- length(x)
m <- 15 + 15*x*cos(4*pi*x)

#Figure A.3 on page 377
hRSW <- dpill(x, y)
h <- hRSW
xx <- c(0:10000)/10000
ticks <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
par(mfrow=c(1,1))
plot( x=c(0,1),y=c(0,35),type="n",xlab="x",ylab="Estimated & True Curves")
axis(1,at=ticks,labels=ticks)
x1 <- 0.5
yy <- (1/(h*sqrt(2*pi)))*exp(-0.5*((x1-x)/h)^2)
lines(x,yy,lty=2)
points(x,y,pch=3,cex=0.5)
lines(x,m,lty=2)
lines(locpoly(x, y, bandwidth = h,degree=1),lty=1)

#Figure A.4 on page 377
par(mfrow=c(2,1))
hlo <- hRSW/5
hhi <- hRSW*5
plot(x,y,xlab="x",ylab="Estimated & True Curves",pch=3,cex=0.5)
lines(locpoly(x, y, bandwidth = hlo,degree=1),lty=1,lwd=1.5)
lines(x,m,lty=2,lwd=1.5)
plot(x,y,xlab="x",ylab="Estimated & True Curves",pch=3,cex=0.5)
lines(locpoly(x, y, bandwidth = hhi,degree=1),lty=1,lwd=1.5)
lines(x,m,lty=2,lwd=1.5)

#Figure A.5 on page 378
loessfit1 <- loess(y~x,span=1/3)
loessfit2 <- loess(y~x,span=2/3)
loessfit3 <- loess(y~x,span=0.05)
par(mfrow=c(1,1))
plot(x,y,xlab="x",ylab="Estimated & True Curves",pch=3,cex=0.5)
lines(x,m,lty=1)
lines(x,predict(loessfit1,data.frame(D=x)),lty=2)

#Figure A.6 on page 379
par(mfrow=c(2,1))
plot(x,y,xlab="x",ylab="Estimated & True Curves",pch=3,cex=0.5)
lines(x,predict(loessfit2,data.frame(D=x)),lty=1,,lwd=1.5)
lines(x,m,lty=2,lwd=1.5)
plot(x,y,xlab="x",ylab="Estimated & True Curves",pch=3,cex=0.5)
lines(x,predict(loessfit3,data.frame(D=x)),lty=1,lwd=1.5)
lines(x,m,lty=2,lwd=1.5)

#Figure A.7 on page 381
library(nlme)
step <- 0.02
knots <- seq(min(x)+step,max(x)-step,by=step)
Z <- outer(x,knots,"-")
Z <- Z*(Z>0)
all <- rep(1,n)
spline <- lme(y~x, random=list(all=pdIdent(~Z-1)))
spline.fit <- fitted(spline)
par(mfrow=c(1,1))
plot(x,y,xlab="x",ylab="Estimated & True Curves",pch=3,cex=0.25)
rug(knots)
lines(x,spline.fit)
lines(x,m,lty=2)

#Figure A.8 on page 382
step <- 0.15
knots <- seq(min(x)+step,max(x)-step,by=step)
Z <- outer(x,knots,"-")
Z <- Z*(Z>0)
all <- rep(1,n)
spline <- lme(y~x, random=list(all=pdIdent(~Z-1)))
spline.fit <- fitted(spline)
par(mfrow=c(1,1))
plot(x,y,xlab="x",ylab="Estimated & True Curves",pch=3,cex=0.25)
rug(knots)
lines(x,spline.fit)
lines(x,m,lty=2)
