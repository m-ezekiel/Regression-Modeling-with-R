#March 17, 2009

#Please change the file path in the command below to coincide with where you have stored the data files
setwd("C:/Users/sheather.ADSTAT/Documents/docs/AModernApproachToRegression/Data")

library(nlme)
FOrthodont <- Orthodont[Orthodont$Sex=="Female",]

#Figure 10.1 on page 333
plot(FOrthodont)

#Output from R on page 334
DistFAge8 <- FOrthodont$distance[FOrthodont$age==8]
DistFAge10 <- FOrthodont$distance[FOrthodont$age==10]
DistFAge12 <- FOrthodont$distance[FOrthodont$age==12]
DistFAge14 <- FOrthodont$distance[FOrthodont$age==14]
T <- cbind(DistFAge8,DistFAge10,DistFAge12,DistFAge14)
c<-cor(T)
round(c,3)

#Figure 10.2 on page 335
pairs(~DistFAge14+DistFAge12+DistFAge10+DistFAge8,lower.panel=NULL)

#Output from R on page 337
mFRI <- lme(distance~age,data=FOrthodont,random=~1|Subject,method="REML")
summary(mFRI)

#Figure 10.3 on page 338
plot(mFRI, form = distance ~ fitted(.) | Subject,layout = c(3,4), between = list(y = c(0, 0, 0, 0.5)),
aspect = 1.0, abline = c(0,1))

#Fixed intercepts in Table 10.2 on page 338
mFFI <- lm(distance~factor(Subject)+age - 1,data=FOrthodont)
summary(mFFI)

#Random intercepts in Table 10.2 on page 338
coef(mFRI)

#Figure 10.4 on page 339
MOrthodont <- Orthodont[Orthodont$Sex=="Male",]
plot(MOrthodont)

#Output from R on pages 340 and 341
DistMAge8 <- MOrthodont$distance[MOrthodont$age==8]
DistMAge10 <- MOrthodont$distance[MOrthodont$age==10]
DistMAge12 <- MOrthodont$distance[MOrthodont$age==12]
DistMAge14 <- MOrthodont$distance[MOrthodont$age==14]
T <- cbind(DistMAge8,DistMAge10,DistMAge12,DistMAge14)
c<-cor(T)
round(c,3)

#Figure 10.5 on page 340
pairs(~DistMAge14+DistMAge12+DistMAge10+DistMAge8,lower.panel=NULL)

#Output from R on page 341
mMRI <- lme(distance~age,data=MOrthodont,random=~1|Subject,method="REML")
summary(mMRI)

#Figure 10.6 on page 342
plot(mMRI, form = distance ~ fitted(.) | Subject,layout = c(4,4), between = list(y = c(0, 0, 0, 0.5)),
aspect = 1.0, abline = c(0,1))

#Output from R on pages 343 and 344
m10.5 <- lme(distance~age*Sex,data=Orthodont,random=~1|Subject,method="REML",
weights=varIdent(form=~1|Sex))
summary(m10.5)

#Output from R on pages 344 and 345
m10.6 <- lme(distance~age*Sex,data=Orthodont,random=~1|Subject,method="REML")
summary(m10.6)

#Output from R on page 345
anova(m10.6,m10.5)

#Figure 10.7 on page 347
qqnorm(m10.5,~ranef(.),id=0.05,cex=0.7)

#Output from R on page 347
MRes <- resid(m10.5, level=0)
MRAge8 <- MRes[Orthodont$age==8]
MRAge10 <- MRes[Orthodont$age==10]
MRAge12 <- MRes[Orthodont$age==12]
MRAge14 <- MRes[Orthodont$age==14]
T <- cbind(MRAge8,MRAge10,MRAge12,MRAge14)
c<-cor(T)
round(c,3)

#Figure 10.8 on page 348
pairs(~MRAge14+MRAge12+MRAge10+MRAge8,lower.panel=NULL)

#Output from R on page 348
CRes <- resid(m10.5, level=1)
CRAge8  <- CRes[Orthodont$age==8]
CRAge10 <- CRes[Orthodont$age==10]
CRAge12 <- CRes[Orthodont$age==12]
CRAge14 <- CRes[Orthodont$age==14]
T <- cbind(CRAge8,CRAge10,CRAge12,CRAge14)
c<-cor(T)
round(c,3)

#Figure 10.9 on page 349
pairs(~CRAge14+CRAge12+CRAge10+CRAge8,lower.panel=NULL)

#Figure 10.10 on page 350
plot(m10.6,form = resid(., type = "response",level=1) ~ fitted(.,level=1) | Sex,abline=0)

#Figure 10.11 on page 351
plot(m10.5,form = resid(., type = "response",level=1) ~ fitted(.,level=1) | Sex,abline=0)


#Figure 10.12 on page 352
#Choleski Residuals for m10.5
attach(Orthodont)
m10.5.a <- lm(distance~age*Sex, data=Orthodont)
m10.5.b <- lm(distance~(Subject-1), data=Orthodont)
m10.5.X <- model.matrix(m10.5.a)
m10.5.Z <- model.matrix(m10.5.b)
m10.5.G <- diag(rep(getVarCov(m10.5),ncol(m10.5.Z)))
m10.5.R <- diag(attr(m10.5[[15]],"std")^2)
m10.5.V <- m10.5.Z %*% m10.5.G %*% t(m10.5.Z) + m10.5.R
m10.5.tCHOLinv <- solve(t(chol(m10.5.V)))
# Now to premultiply terms by m10.5.tCHOLinv to get Choleski residuals, etc.
dist.CHOL.m10.5<- m10.5.tCHOLinv %*% Orthodont$distance
x1.CHOL.m10.5 <- m10.5.tCHOLinv %*% m10.5.X[,1] 
x2.CHOL.m10.5 <- m10.5.tCHOLinv %*% m10.5.X[,2] 
x3.CHOL.m10.5 <- m10.5.tCHOLinv %*% m10.5.X[,3] 
x4.CHOL.m10.5 <- m10.5.tCHOLinv %*% m10.5.X[,4] 
m10.5.CHOL <- lm(dist.CHOL.m10.5 ~ x1.CHOL.m10.5 + x2.CHOL.m10.5 +
 x3.CHOL.m10.5 + x4.CHOL.m10.5 -1)
#summary(m10.5.CHOL)

#Choleski Residuals for m10.6 
m10.6.a <- lm(distance~age*Sex, data=Orthodont)
m10.6.b <- lm(distance~(Subject-1), data=Orthodont)
m10.6.X <- model.matrix(m10.6.a)
m10.6.Z <- model.matrix(m10.6.b)
m10.6.G <- diag(rep(getVarCov(m10.6),ncol(m10.6.Z)))
m10.6.R <- diag(attr(m10.6[[15]],"std")^2)
m10.6.V <- m10.6.Z %*% m10.6.G %*% t(m10.6.Z) + m10.6.R
m10.6.tCHOLinv <- solve(t(chol(m10.6.V)))
# Now to premultiply terms by m10.6.tCHOLinv to get Choleski residuals, etc.
dist.CHOL.m10.6<- m10.6.tCHOLinv %*% Orthodont$distance
x1.CHOL.m10.6 <- m10.6.tCHOLinv %*% m10.6.X[,1] 
x2.CHOL.m10.6 <- m10.6.tCHOLinv %*% m10.6.X[,2] 
x3.CHOL.m10.6 <- m10.6.tCHOLinv %*% m10.6.X[,3] 
x4.CHOL.m10.6 <- m10.6.tCHOLinv %*% m10.6.X[,4] 
m10.6.CHOL <- lm(dist.CHOL.m10.6 ~ x1.CHOL.m10.6 + x2.CHOL.m10.6 +
 x3.CHOL.m10.6 + x4.CHOL.m10.6 -1)
#summary(m10.6.CHOL)

CholeskyResid10.5 <- m10.5.CHOL$residuals
CholeskyResid10.6 <- m10.6.CHOL$residuals

par(mfrow=c(1,2))
plot(CholeskyResid10.6~Sex,ylab="Cholesky residuals from model (10.6)",ylim=c(-3,4))
plot(CholeskyResid10.5~Sex,ylab="Cholesky residuals from model (10.5)",ylim=c(-3,4))

#p-value for Levene's test on page 353
library(car)
levene.test(CholeskyResid10.6,Sex)

detach(Orthodont)


library(foreign)
library(nlme)

pigweights <- read.csv("pigweights.csv",header=TRUE)
pigweights$Animal <- pigweights$pigid
pigweights$Time <- pigweights$weeknumber
attach(pigweights)

pigweights <- groupedData(weight ~ Time | Animal,data = pigweights,
labels = list(x = "Time",y="Weight"),
units = list(x = "(weeks)", y = "(kg)"))

#Figure 10.13 on page 354
par(mfrow=c(1,1))
plot(Time,weight)
lines(Time,weight,lty=2)

#Output from R on page 354
T1 <- weight[Time==1]
T2 <- weight[Time==2]
T3 <- weight[Time==3]
T4 <- weight[Time==4]
T5 <- weight[Time==5]
T6 <- weight[Time==6]
T7 <- weight[Time==7]
T8 <- weight[Time==8]
T9 <- weight[Time==9]
T <- cbind(T1,T2,T3,T4,T5,T6,T7,T8,T9)
c<-cor(T)
round(c,2)

#Figure 10.14 on page 355
pairs(~T9+T8+T7+T6+T5+T4+T3+T2+T1,lower.panel=NULL)

#Output from R on page 356 and 357
m10p13 <- gls(weight~Time,data=pigweights,
 correlation=corSymm(form=~1|Animal),weights=varIdent(form=~1|Time),method="REML")
summary(m10p13)

#Choleski Residuals for m10.13 
m10p13.V <- getVarCov(m10p13)
m10p13.tCHOLinv <- diag(1,length(unique(pigweights$Animal))) %x% solve(t(chol(m10p13.V)))
ystar <- m10p13.tCHOLinv %*% pigweights$weight
Intstar <- m10p13.tCHOLinv %*% rep(1,length(pigweights$Time))
Timestar <- m10p13.tCHOLinv %*% pigweights$Time
m10p13star <- lm(ystar~Intstar+Timestar-1)
summary(m10p13star)
Resm10p13star <- m10p13star$residuals

#Figure 10.15 on page 358
par(mfrow=c(1,1))
plot(Timestar,Resm10p13star,ylab="Cholesky Residuals",xlab="x*",ylim=c(-3.5,3.5))
lines(lowess(Timestar,Resm10p13star,iter=1,f=1/3),lty=2)

#F-statistic p-value on page 357
mres <- lm(Resm10p13star~Timestar+I(Timestar^2)+I(Timestar^3)+I(Timestar^4)+I(Timestar^5))
summary(mres)

#Output from R on pages 360 and 361: REML fit of model (10.15)
m10p15 <- gls(weight~Time+TimeM2Plus+TimeM3Plus+TimeM4Plus+TimeM5Plus+TimeM6Plus+TimeM7Plus+TimeM8Plus,
data=pigweights,correlation=corSymm(form=~1|Animal),weights=varIdent(form=~1|Time),method="REML")
summary(m10p15)

#Output from R on page 361: Comparing ML fits of models (10.13) and (10.15)
m10p15.ML <- gls(weight~Time+TimeM2Plus+TimeM3Plus+TimeM4Plus+TimeM5Plus+TimeM6Plus+TimeM7Plus+TimeM8Plus,
data=pigweights,correlation=corSymm(form=~1|Animal),weights=varIdent(form=~1|Time),method="ML")
m10p13.ML <- gls(weight~Time,data=pigweights,
 correlation=corSymm(form=~1|Animal),weights=varIdent(form=~1|Time),method="ML")
anova(m10p13.ML,m10p15.ML)

#Output from R on page 363: Comparing ML fits of models (10.15) and (10.16)
m10p16.ML <- gls(weight~Time+TimeM3Plus+TimeM7Plus+TimeM8Plus,
data=pigweights,correlation=corSymm(form=~1|Animal),weights=varIdent(form=~1|Time),method="ML")
anova(m10p16.ML,m10p15.ML)

#Output from R on page 363: REML fit of model (10.16)
m10p16 <- gls(weight~Time+TimeM3Plus+TimeM7Plus+TimeM8Plus,
data=pigweights,correlation=corSymm(form=~1|Animal),weights=varIdent(form=~1|Time),method="REML")
summary(m10p16)

#Output from R on page 363: Comparing ML fits of models (10.16) and (10.17)
m10p17.ML <- gls(weight~Time+TimeM3Plus+TimeM5Plus+TimeM7Plus+TimeM8Plus,
data=pigweights,correlation=corSymm(form=~1|Animal),weights=varIdent(form=~1|Time),method="ML")
anova(m10p17.ML,m10p16.ML)

#Output from R on page 365: REML fit of model (10.16) with AR errors
m10p16.AR1 <- gls(weight~Time+TimeM3Plus+TimeM7Plus+TimeM8Plus,
data=pigweights,correlation=corAR1(form=~1|Animal),weights=varIdent(form=~1|Time),method="REML")
summary(m10p16.AR1)

#Output from R on page 365: Comparing ML fits of models (10.16) with different errors
anova(m10p16.AR1,m10p16)

detach(pigweights)

