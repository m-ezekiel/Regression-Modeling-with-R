#March 17, 2009

#Please change the file path in the command below to coincide with where you have stored the data files
setwd("C:/Users/sheather.ADSTAT/Documents/docs/AModernApproachToRegression/Data")

production <- read.table("production.txt",header=TRUE)
attach(production)

#Figure 2.1 on page 16
par(mfrow=c(1,1))
plot(production$RunSize,production$RunTime,xlab="Run Size", ylab="Run Time")

#R output on page 19
m1 <- lm(RunTime~RunSize)
summary(m1)

#Figure 2.3 on page 20
plot(production$RunSize,production$RunTime,xlab="Run Size", ylab="Run Time")
abline(lsfit(production$RunSize,production$RunTime))

#t-value on page 23
tval <- qt(1-0.05/2,18)
tval

#95% confidence intervals on page 24
round(confint(m1,level=0.95),3)

#R output on page 27
predict(m1,newdata=data.frame(RunSize=c(50,100,150,200,250,300,350)),interval="confidence",level=0.95)
predict(m1,newdata=data.frame(RunSize=c(50,100,150,200,250,300,350)),interval="prediction",level=0.95)

#R output on page 30
anova(m1)
detach(production)


changeover_times <- read.table("changeover_times.txt",header=TRUE)
attach(changeover_times)

#R output on page 31
m1 <- lm(Changeover~New)
summary(m1)

#Figure 2.5 on page 32
par(mfrow=c(2,2))
plot(New,Changeover,xlab="Dummy variable, New",ylab="Change Over Time")
abline(lsfit(New,Changeover))
boxplot(Changeover~New,xlab="Dummy variable, New",ylab="Change Over Time")
boxplot(Changeover~Method,ylab="Change Over Time",xlab="Method")

#t-value on page 33
tval <- qt(1-0.05/2,118)
tval

detach(changeover_times)

#################EXERCISES

#Exercise 2.8.1

playbill <- read.csv("C:playbill.csv",header=TRUE)
attach(playbill)

#Figure 2.6 on page 38
par(mfrow=c(1,1))
plot(LastWeek,CurrentWeek,xlab="Gross box office results previous week",
ylab="Gross box office results current week")

detach(playbill)

#Exercise 2.8.3

invoice <- read.table("invoices.txt",header=TRUE)
attach(invoice)

#Figure 2.7 on page 40
par(mfrow=c(1,1))
plot(Invoices,Time,xlab="Number of Invoices",ylab="Processing Time")
abline(lsfit(Invoices,Time))

#Output from R on page 40
m1 <- lm(Time~Invoices)
summary(m1)
mean(Time)
median(Time)
mean(Invoices)
median(Invoices)

detach(invoice)

#Exercise 2.8.5
problem5 <- read.table("Ch2Problem5.txt",header=TRUE)
attach(problem5)

#Figure 2.8 on page 42
par(mfrow=c(1,2))
plot(x1,y,main="Model 1")
abline(lsfit(x1,y))
plot(x2,y,main="Model 2")
abline(lsfit(x2,y))

detach(problem5)


