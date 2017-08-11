#March 17, 2009

#Please change the file path in the command below to coincide with where you have stored the data files
setwd("C:/Users/sheather.ADSTAT/Documents/docs/AModernApproachToRegression/Data")

profsalary <- read.table("profsalary.txt",header=TRUE)
attach(profsalary)

#Figure 5.1 on page 126
plot(Experience,Salary,xlab="Years of Experience")

#Figure 5.2 on page 127
m1 <- lm(Salary~Experience)
leverage1 <- hatvalues(m1)
StanRes1 <- rstandard(m1)
ExperienceNew <- seq(0,37,len=37)
plot(Experience,StanRes1,xlab="Years of Experience", ylab="Standardized Residuals")

#Figure 5.3 on page 127
m2 <- lm(Salary~Experience + I(Experience^2))
plot(Experience,Salary,xlab="Years of Experience")
ExperienceNew <- seq(0,37,len=37)
lines(ExperienceNew,predict(m2,newdata=data.frame(Experience=ExperienceNew)))

#Figure 5.4 on page 128
StanRes2 <- rstandard(m2)
plot(Experience,StanRes2,xlab="Years of Experience", ylab="Standardized Residuals")

#Figure 5.5 on page 128
leverage2 <- hatvalues(m2)
plot(Experience,leverage2,xlab="Years of Experience",ylab="Leverage")
abline(h=6/max(Case),lty=2)

#Figure 5.6 on page 129
par(mfrow=c(2,2))
plot(m2)

#Regression output on pages 129 & 130
summary(m2)
predict(m2,newdata=data.frame(Experience=c(10)),interval="prediction",level=0.95)
detach(profsalary)

nyc <- read.csv("nyc.csv", header=TRUE)
attach(nyc)

#Regression output on pages 138 & 139
m1 <- lm(Price~Food+Decor+Service+East)
summary(m1)

#Regression output on page 139
m2 <- lm(Price~Food+Decor+East)
summary(m2)
#An alterntive way to obtain m2 is to use the update command
m2 <- update(m1,~.-Service)
summary(m2)

detach(nyc)

travel <- read.table("travel.txt",header=TRUE)
attach(travel)

#Regression output on page 141
mfull <- lm(Amount~Age+C+C:Age)
summary(mfull)

#Figure 5.7 on page 142
par(mfrow=c(1,1))
plot(Age[C==0],Amount[C==0],pch=c("A"),col=c("black"),ylab="Amount Spent",xlab="Age")
points(Age[C==1],Amount[C==1],pch=c("C"),col=c("red"))

#Regression output on page 143
mreduced <- lm(Amount~Age)
summary(mreduced)

#Regression output on page 144
anova(mreduced,mfull)

detach(travel)


nyc <- read.csv("nyc.csv", header=TRUE)
attach(nyc)

#Regression output on page 145
mfull <- lm(Price~Food+Decor+Service+East+Food:East+Decor:East+Service:East)
summary(mfull)

#Regression output on page 146
mreduced <- lm(Price~Food+Decor+East)
summary(mreduced)

#Regression output on page 146
anova(mreduced,mfull)

detach(nyc)

#################EXERCISES

#Ex 5.4.3

latour <- read.table("Latour.txt", header=TRUE)
attach(latour)

#Regression output on page 148
mfull <- lm(Quality ~ EndofHarvest + Rain + Rain:EndofHarvest)
summary(mfull)

#Figure 5.8 on page 149
y = Rain
par(mfrow=c(1,1))
plot(EndofHarvest,Quality,pch=y+1,col=y+1,xlab="End of Harvest (in days since August 31)")
abline(lsfit(EndofHarvest[y==0],Quality[y==0]),lty=1,col=1)
abline(lsfit(EndofHarvest[y==1],Quality[y==1]),lty=2,col=2)
legend(23, 2.5,legend=c("No","Yes"),pch=1:2,col=1:2,title="Rain at Harvest?")

#Regression output on page 149
mreduced <- lm(Quality ~ EndofHarvest + Rain)
summary(mreduced)
anova(mreduced,mfull)

detach(latour)