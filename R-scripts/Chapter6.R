#March 17, 2009

#Please change the file path in the command below to coincide with where you have stored the data files
setwd("C:/Users/sheather.ADSTAT/Documents/docs/AModernApproachToRegression/Data")

nyc <- read.csv("nyc.csv", header=TRUE)
attach(nyc)

#Figure 6.1 on page 157
pairs(~Food+Decor+Service,data=nyc,gap=0.4,cex.labels=1.5)

#Figure 6.2 on page 158
m1 <- lm(Price~Food+Decor+Service+East)
summary(m1)
StanRes1 <- rstandard(m1)
par(mfrow=c(2,2))
plot(Food,StanRes1, ylab="Standardized Residuals")
plot(Decor,StanRes1, ylab="Standardized Residuals")
plot(Service,StanRes1, ylab="Standardized Residuals")
plot(East,StanRes1, ylab="Standardized Residuals")

#Figure 6.3 on page 158
par(mfrow=c(1,1))
plot(m1$fitted.values,Price,xlab="Fitted Values", ylab="Price")
abline(lsfit(m1$fitted.values,Price))
detach(nyc)

#Figure 6.4 on page 160
library(alr3)
data(caution)
attach(caution)
pairs(y~x1+x2)

#Figure 6.5 on page 160
m1 <- lm(y~x1+x2)
StanRes1 <- rstandard(m1)
par(mfrow=c(2,2))
plot(x1,StanRes1, ylab="Standardized Residuals")
plot(x2,StanRes1, ylab="Standardized Residuals")
plot(m1$fitted.values,StanRes1, ylab="Standardized Residuals",xlab="Fitted Values")

#Figure 6.6 on page 161
par(mfrow=c(1,1))
plot(m1$fitted.values,y,xlab="Fitted Values")
abline(lsfit(m1$fitted.values,y))

detach(caution)


nonlinearx <- read.table("nonlinearx.txt",header=TRUE)
attach(nonlinearx)

#Figure 6.7 on page 162
par(mfrow=c(2,2))
plot(x1,y)
plot(x2,y)
plot(x1,x2)

#Figure 6.8 on page 163
m1 <- lm(y~x1+x2)
summary(m1)
StanRes1 <- rstandard(m1)
par(mfrow=c(2,2))
plot(x1,StanRes1, ylab="Standardized Residuals")
plot(x2,StanRes1, ylab="Standardized Residuals")
plot(m1$fitted.values,StanRes1,xlab="Fitted Values",ylab="Standardized Residuals")

detach(nonlinearx)


nyc <- read.csv("nyc.csv", header=TRUE)
attach(nyc)

#Figure 6.9 on page 165
par(mfrow=c(2,2))
plot(Food,Price)
abline(lsfit(Food,Price))
plot(Decor,Price)
abline(lsfit(Decor,Price))
plot(Service,Price)
abline(lsfit(Service,Price))
plot(East,Price)
abline(lsfit(East,Price))

#Figure 6.10 on page 166
install.packages("car")
#You will be asked to 
#--- Please select a CRAN mirror for use in this session ---
library(car)
m1 <- lm(Price~Food+Decor+Service+East)
par(mfrow=c(2,2))
avp(m1,variable=Food,ask=FALSE,identify.points=TRUE)
# Click on the points you wish to identify. When you wish
# to stop click the right mouse button and select "Stop"
avp(m1,variable=Decor,ask=FALSE,identify.points=FALSE)
avp(m1,variable=Service,ask=FALSE,identify.points=FALSE)
avp(m1,variable=East,ask=FALSE,identify.points=FALSE)

detach(nyc)


defects <- read.table("defects.txt", header=TRUE)
attach(defects)

#Figure 6.11 on page 169
pairs(Defective ~ Temperature+Density+Rate)

#Figure 6.12 on page 170
m1 <- lm(Defective ~ Temperature+Density+Rate)
par(mfrow=c(2,2))
StanRes1 <- rstandard(m1)
plot(Temperature,StanRes1,ylab="Standardized Residuals")
plot(Density,StanRes1,ylab="Standardized Residuals")
plot(Rate,StanRes1,ylab="Standardized Residuals")
plot(m1$fitted.values,StanRes1,ylab="Standardized Residuals",xlab="Fitted Values")

#Figure 6.13 on page 170
par(mfrow=c(1,1))
fit1 <- m1$fitted.values
m2 <- lm(Defective~fit1 + I(fit1^2))
plot(fit1,Defective,xlab="Fitted Values")
fitnew <- seq(-15,60,len=76)
lines(fitnew,predict(m2,newdata=data.frame(fit1=fitnew)))
abline(lsfit(m1$fitted.values,Defective),lty=2)

#Figure 6.14 on page 171
library(alr3)
inverse.response.plot(m1,key=TRUE)

#Figure 6.15 on page 173
library(MASS)
boxcox(m1,lambda=seq(0.3,0.65,length=20))

#Figure 6.16 on page 173
par(mfrow=c(2,2))
plot(Temperature,sqrt(Defective),ylab=expression(sqrt(Defective)))
plot(Density,sqrt(Defective),ylab=expression(sqrt(Defective)))
plot(Rate,sqrt(Defective),ylab=expression(sqrt(Defective)))

#Figure 6.17 on page 174
mt <- lm(sqrt(Defective) ~ Temperature+Density+Rate)
par(mfrow=c(2,2))
StanRest <- rstandard(mt)
plot(Temperature,StanRest,ylab="Standardized Residuals")
plot(Density,StanRest,ylab="Standardized Residuals")
plot(Rate,StanRest,ylab="Standardized Residuals")
plot(mt$fitted.values,StanRest,ylab="Standardized Residuals",xlab="Fitted Values")

#Figure 6.18 on page 174
par(mfrow=c(1,1))
plot(mt$fitted.values,sqrt(Defective),xlab="Fitted Values",ylab=expression(sqrt(Defective)))
abline(lsfit(mt$fitted.values,sqrt(Defective)))

#Figure 6.19 on page 175
par(mfrow=c(2,2))
plot(mt)

#Regression output on page 175
summary(mt)

#Figure 6.20 on page 176
library(car)
par(mfrow=c(2,2))
avp(mt,variable=Temperature,ask=FALSE,identify.points=FALSE)
avp(mt,variable=Density,ask=FALSE,identify.points=FALSE)
avp(mt,variable=Rate,ask=FALSE,identify.points=FALSE)

detach(defects)


magazines <- read.csv("magazines.csv", header=TRUE)
attach(magazines)

#R output on page 177
library(alr3)
summary(bctrans(AdRevenue~AdPages+SubRevenue+NewsRevenue))

#Figure 6.21 on page 178
pairs(AdRevenue~AdPages+SubRevenue+NewsRevenue)

#Figure 6.22 on page 179
tAdPages<- log(AdPages)
tSubRevenue <- log(SubRevenue)
tNewsRevenue <- log(NewsRevenue)
m1 <- lm(AdRevenue~log(AdPages)+log(SubRevenue)+log(NewsRevenue))
library(alr3)
par(mfrow=c(1,1))
inverse.response.plot(m1,key=TRUE)

#R output on page 179
library(alr3)
summary(tranxy <- bctrans(~AdRevenue+AdPages+SubRevenue+NewsRevenue))

#Figure 6.23 on page 180
pairs(log(AdRevenue)~log(AdPages)+log(SubRevenue)+log(NewsRevenue))

#Figure 6.24 on page 181
m2 <- lm(log(AdRevenue)~log(AdPages)+log(SubRevenue)+log(NewsRevenue))
par(mfrow=c(2,2))
StanRes2 <- rstandard(m2)
plot(log(AdPages),StanRes2,ylab="Standardized Residuals")
plot(log(SubRevenue),StanRes2,ylab="Standardized Residuals")
plot(log(NewsRevenue),StanRes2,ylab="Standardized Residuals")
plot(m2$fitted.values,StanRes2,ylab="Standardized Residuals",xlab="Fitted Values")

#Figure 6.25 on page 181
par(mfrow=c(1,1))
plot(m2$fitted.values,log(AdRevenue),xlab="Fitted Values")
abline(lsfit(m2$fitted.values,log(AdRevenue)))

#Figure 6.26 on page 182
par(mfrow=c(2,2))
plot(m2)
abline(v=2*4/204,lty=2)

#Figure 6.27 on page 183
library(car)
par(mfrow=c(2,2))
avp(m2,variable=log(AdPages),ask=FALSE,identify.points=FALSE)
avp(m2,variable=log(SubRevenue),ask=FALSE,identify.points=FALSE)
avp(m2,variable=log(NewsRevenue),ask=FALSE,identify.points=FALSE)

#Regression output on page 183
summary(m2)

detach(magazines)


circulation <- read.table("circulation.txt", header=TRUE, sep="\t")
attach(circulation)

#Figure 6.28 on page 185
par(mfrow=c(1,1))
plot(log(Weekday),log(Sunday),xlab="log(Weekday Circulation)",ylab="log(Sunday Circulation)",
pch=Tabloid.with.a.Serious.Competitor+1,col=Tabloid.with.a.Serious.Competitor+1)
legend(11.6, 14.1,legend=c("0","1"),pch=1:2,col=1:2,title="Tabloid dummy variable")

#Figure 6.29 on page 186
m1 <- lm(log(Sunday) ~ log(Weekday) + Tabloid.with.a.Serious.Competitor)
par(mfrow=c(2,2))
StanRes1 <- rstandard(m1)
plot(log(Weekday),StanRes1,ylab="Standardized Residuals",xlab="log(Sunday Circulation)")
plot(Tabloid.with.a.Serious.Competitor,StanRes1,ylab="Standardized Residuals")
plot(m1$fitted.values,StanRes1,ylab="Standardized Residuals",xlab="Fitted Values")

#Figure 6.30 on page 186
par(mfrow=c(1,1))
plot(m1$fitted.values,log(Sunday),xlab="Fitted Values",ylab="log(Sunday Circulation)")
abline(lsfit(m1$fitted.values,log(Sunday)))

#Figure 6.31 on page 187
par(mfrow=c(2,2))
plot(m1)
abline(v=2*3/89,lty=2)

#Regression output on page 188 
summary(m1)

#R output on page 188
predict(m1,newdata=data.frame(
Weekday=c(210000),Tabloid.with.a.Serious.Competitor=c(1)),interval="prediction",level=0.95)
predict(m1,newdata=data.frame(
Weekday=c(210000),Tabloid.with.a.Serious.Competitor=c(0)),interval="prediction",level=0.95)

#Figure 6.32 on page 189
library(car)
par(mfrow=c(1,2))
avp(m1,variable=log(Weekday),ask=FALSE,identify.points=FALSE)
avp(m1,variable=Tabloid.with.a.Serious.Competitor,ask=FALSE,identify.points=FALSE)

detach(circulation)


profsalary <- read.table("profsalary.txt",header=TRUE)
attach(profsalary)

#Figure 6.33 on page 190
library(alr3)
m1 <- lm(Salary~Experience)
par(mfrow=c(1,1))
mmp(m1,Experience,xlab="Years of Experience",key=NULL)

#Figure 6.34 on page 191
m2 <- lm(Salary~Experience + I(Experience^2))
mmp(m2,Experience,xlab="Years of Experience",key=NULL)

detach(profsalary)


defects <- read.table("defects.txt", header=TRUE)
attach(defects)

#Figure 6.35 on page 193
m1 <- lm(Defective ~ Temperature+Density+Rate)
loessfit1 <- loess(Defective ~ Temperature,degree=1,span=2/3)
loessfit2 <- loess(m1$fitted.values ~ Temperature,degree=1,span=2/3)
xx <- seq(min(Temperature),max(Temperature),length=100)
par(mfrow=c(1,2))
plot(Temperature,Defective,xlab="Temperature, x1", ylab="Defective, Y")
lines(xx,predict(loessfit1,data.frame(Temperature=xx)))
plot(Temperature,m1$fitted.values,ylab=expression(hat(Y)),xlab="Temperature, x1")
lines(xx,predict(loessfit2,data.frame(Temperature=xx)))

#Figure 6.36 on page 193
library(alr3)
par(mfrow=c(1,1))
mmp(m1,Temperature)
#Notice that the figure produced has a legend box in the upper left hand corner, that is
#not in the version of Figure 6.36 in the published book. The reason for this is that 
#the figure in the book was produced using an earlier version of the alr3 function 'mmp'.

#Figure 6.37 on page 194
par(mfrow=c(2,2))
mmp(m1,Temperature)
mmp(m1,Density,key="topright")
mmp(m1,Rate)
mmp(m1,m1$fitted.values,xlab="Fitted Values")

#Figure 6.38 on page 195
m2 <- lm(sqrt(Defective) ~ Temperature+Density+Rate)
par(mfrow=c(2,2))
mmp(m2,Temperature)
mmp(m2,Density,key="topright")
mmp(m2,Rate)
mmp(m2,m2$fitted.values,xlab="Fitted Values")

detach(defects)

 
bridge <- read.table("bridge.txt", header=TRUE)
attach(bridge)

#R output on page 196
library(alr3)
summary(tranxy <- bctrans(~Time+DArea+CCost+Dwgs+Length+Spans))

#Figure 6.39 page 197
pairs(Time~DArea+CCost+Dwgs+Length+Spans,data=bridge,cex.labels=1.4)

#Figure 6.40 page 198
pairs(log(Time)~log(DArea)+log(CCost)+log(Dwgs)+log(Length)+log(Spans),data=bridge)

#Figure 6.41 page 199
m1 <- lm(log(Time)~log(DArea)+log(CCost)+log(Dwgs)+log(Length)+log(Spans))
StanRes1 <- rstandard(m1)
par(mfrow=c(2,3))
plot(log(DArea),StanRes1, ylab="Standardized Residuals")
plot(log(CCost),StanRes1, ylab="Standardized Residuals")
plot(log(Dwgs),StanRes1, ylab="Standardized Residuals")
plot(log(Length),StanRes1, ylab="Standardized Residuals")
plot(log(Spans),StanRes1, ylab="Standardized Residuals")
plot(m1$fitted.values,StanRes1, ylab="Standardized Residuals",xlab="Fitted values")

#Figure 6.42 page 199
par(mfrow=c(1,1))
plot(m1$fitted.values,log(Time),xlab="Fitted Values")
abline(lsfit(m1$fitted.values,log(Time)))

#Figure 6.43 page 200
par(mfrow=c(2,2))
plot(m1)
abline(v=2*6/45,lty=2)

#Regression output on page 200
summary(m1)

#Figure 6.44 page 201
library(alr3)
mmps(m1,layout=c(2,3))

#R output on page 202
logDArea <- log(DArea)
logCCost <- log(CCost)
logDwgs <- log(Dwgs)
logLength <- log(Length)
logSpans <- log(Spans)
X <- cbind(logDArea,logCCost,logDwgs,logLength,logSpans)
c <- cor(X)
round(c,3)

#Figure 6.45 on page 202
library(car)
par(mfrow=c(2,3))
avp(m1,variable=log(DArea),ask=FALSE,identify.points=FALSE)
avp(m1,variable=log(CCost),ask=FALSE,identify.points=FALSE)
avp(m1,variable=log(Dwgs),ask=FALSE,identify.points=FALSE)
avp(m1,variable=log(Length),ask=FALSE,identify.points=FALSE)
avp(m1,variable=log(Spans),ask=FALSE,identify.points=FALSE)

#R output on page 203
library(car)
vif(m1)

detach(bridge)

Bordeaux <- read.csv("Bordeaux.csv", header=TRUE)
attach(Bordeaux)

#Figure 6.46 on page 205
m1 <- lm(log(Price)~log(ParkerPoints)+log(CoatesPoints)+P95andAbove+FirstGrowth+CultWine+Pomerol+VintageSuperstar)
StanRes1 <- rstandard(m1)
par(mfrow=c(3,3))
plot(log(ParkerPoints),StanRes1, ylab="Standardized Residuals")
plot(log(CoatesPoints),StanRes1, ylab="Standardized Residuals")
boxplot(StanRes1~P95andAbove,ylab="Standardized Residuals",xlab="P95andAbove")
boxplot(StanRes1~FirstGrowth,ylab="Standardized Residuals",xlab="FirstGrowth")
boxplot(StanRes1~CultWine, ylab="Standardized Residuals",xlab="CultWine")
boxplot(StanRes1~Pomerol, ylab="Standardized Residuals",xlab="Pomerol")
boxplot(StanRes1~VintageSuperstar, ylab="Standardized Residuals",xlab="VintageSuperstar")
plot(m1$fitted.values,StanRes1, ylab="Standardized Residuals",xlab="Fitted values")

#Figure 6.47 on page 205
par(mfrow=c(1,1))
plot(m1$fitted.values,log(Price),xlab="Fitted Values")
abline(lsfit(m1$fitted.values,log(Price)))

#Figure 6.48 on page 206
par(mfrow=c(2,2))
plot(m1)
abline(v=2*8/72,lty=2)

#Regression output on page 206
summary(m1)

#R output on page 206
library(car)
vif(m1)

#Figure 6.49 on page 207
library(alr3)
par(mfrow=c(2,2))
mmp(m1,log(ParkerPoints))
mmp(m1,log(CoatesPoints))
mmp(m1,m1$fitted.values,xlab="Fitted Values")

#Figure 6.50 on page 208
library(car)
par(mfrow=c(2,4))
avp(m1,variable=log(ParkerPoints),ask=FALSE,identify.points=TRUE, main="")
avp(m1,variable=log(CoatesPoints),ask=FALSE,identify.points=TRUE, main="")
avp(m1,variable=P95andAbove,ask=FALSE,identify.points=FALSE, main="")
avp(m1,variable=FirstGrowth,ask=FALSE,identify.points=FALSE, main="")
avp(m1,variable=CultWine,ask=FALSE,identify.points=FALSE, main="")
avp(m1,variable=Pomerol,ask=FALSE,identify.points=FALSE, main="")
avp(m1,variable=VintageSuperstar,ask=FALSE,identify.points=FALSE, main="")

#Regression output on pages 208-209
m2 <- lm(log(Price)~log(ParkerPoints)+log(CoatesPoints)+FirstGrowth+CultWine+Pomerol+VintageSuperstar)
summary(m2)
anova(m2,m1)

detach(Bordeaux)


storks <- read.table("storks.txt",header=TRUE)
attach(storks)

#Figure 6.51 on page 211
par(mfrow=c(1,1))
plot(Storks,Babies,xlab="Number of Storks",ylab="Number of Babies")
abline(lsfit(Storks,Babies))

#Regression output on page 212
m1 <- lm(Babies ~  Storks,data=storks)
summary(m1)

#Figure 6.52 on page 212
par(mfrow=c(2,2))
plot(Storks,Babies,xlab="Number of Storks",ylab="Number of Babies")
abline(lsfit(Storks,Babies))
plot(Women,Babies,xlab="Number of Women",ylab="Number of Babies")
abline(lsfit(Women,Babies))
plot(Storks,Women,xlab="Number of Storks",ylab="Number of Women")
abline(lsfit(Storks,Women))

#Regression output on page 213
m2 <- lm(Babies ~  Women + Storks,data=storks)
summary(m2)

detach(storks)

#################EXERCISES

#Exercise 6.7.3

cars04 <- read.csv("cars04.csv",header=TRUE)
attach(cars04)

#Figure 6.53 on page 217
pairs(~EngineSize+Cylinders+Horsepower+HighwayMPG+Weight+WheelBase+Hybrid,
data=cars04,gap=0.4,cex.labels=0.85)

#Output from R for model (6.36) on pages 217 and 218
m1 <- lm(SuggestedRetailPrice~EngineSize+Cylinders+Horsepower+HighwayMPG+Weight+WheelBase+Hybrid)
summary(m1)
library(alr3)
summary(tranxy <- 
bctrans(~EngineSize+Cylinders+Horsepower+HighwayMPG+Weight+WheelBase))

#Figure 6.54 on page 218
par(mfrow=c(2,2))
plot(m1)

#Output from R for model (6.37) on pages 219 and 220
tSuggestedRetailPrice <- log(SuggestedRetailPrice)
tEngineSize <- EngineSize^0.25
tCylinders <- log(Cylinders)
tHorsepower <- log(Horsepower)
tHighwayMPG <- 1/HighwayMPG
tWheelBase <- log(WheelBase)
m2 <- lm(tSuggestedRetailPrice~tEngineSize+tCylinders+tHorsepower+tHighwayMPG+Weight+tWheelBase+Hybrid)
summary(m2)
#Note that the output that appears in the book is incorrect as it does not coincide what is
#produced by the previous commands.

#Figure 6.55 on page 219
pairs(~tEngineSize+tCylinders+tHorsepower+tHighwayMPG+Weight+tWheelBase+Hybrid,
data=cars04,gap=0.4,cex.labels=0.82)

#Figure 6.56 on page 220
par(mfrow=c(2,2))
plot(m2)

#Output from R for model (6.37) on page 220
library(car)
round(vif(m2),2)

#Regression output on pages 220 and 221
m3 <- lm(tSuggestedRetailPrice~tEngineSize+tCylinders+tHorsepower+Weight+Hybrid)
summary(m3)

#Figure 6.57 on page 221
library(alr3)
par(mfrow=c(3,3))
mmp(m2,tEngineSize,key=NULL)
mmp(m2,tCylinders,key=NULL)
mmp(m2,tHorsepower,key=NULL)
mmp(m2,tHighwayMPG,key=NULL)
mmp(m2,Weight,key=NULL)
mmp(m2,tWheelBase,key=NULL)
mmp(m2,m2$fitted.values,xlab="Fitted Values",key=NULL)

detach(cars04)


#Exercise 6.7.4

krafft <- read.table("krafft.txt",header=TRUE)
attach(krafft)

#Figure 6.58 on page 222
pairs(KPOINT~RA+VTINV+DIPINV+HEAT,data=krafft,gap=0.4)

#Output from R for model (6.38) on page 223 and 224
m1 <- lm(KPOINT~RA+VTINV+DIPINV+HEAT)
summary(m1)
library(car)
vif(m1)

#Figure 6.59 on page 223
par(mfrow=c(2,2))
plot(m1)
par(mfrow=c(2,2))
plot(m1)

#Figure 6.60 on page 224
leverage1 <- hatvalues(m1)
StanRes1 <- rstandard(m1)
plot(RA,StanRes1, ylab="Standardized Residuals")
plot(VTINV,StanRes1, ylab="Standardized Residuals")
plot(DIPINV,StanRes1, ylab="Standardized Residuals")
plot(HEAT,StanRes1, ylab="Standardized Residuals")

detach(krafft)