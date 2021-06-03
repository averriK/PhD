# Prediction-Continuous: Polynomials & Splines

setwd("C:/")
library(readxl)   # read Excel files

rm(list=ls())   #clean memory

# read data
dat <- read_excel("Pred001 Sales1dim.xlsx")
dat$P2 <- dat$pricegap^2
dat$P3 <- dat$pricegap^3
dat$P4 <- dat$pricegap^4
dat$P5 <- dat$pricegap^5
dat$P6 <- dat$pricegap^6
dat$P7 <- dat$pricegap^7
dat$P8 <- dat$pricegap^8
dat$P9 <- dat$pricegap^9
datL <- dat[1:10,]
datT <- dat[11:13,]
# Graph base
pricegap<-seq(-1.8,1.3,0.01)
datgraf<-data.frame(pricegap)
datgraf$P2 <- datgraf$pricegap^2
datgraf$P3 <- datgraf$pricegap^3
datgraf$P4 <- datgraf$pricegap^4
datgraf$P5 <- datgraf$pricegap^5
datgraf$P6 <- datgraf$pricegap^6
datgraf$P7 <- datgraf$pricegap^7
datgraf$P8 <- datgraf$pricegap^8
datgraf$P9 <- datgraf$pricegap^9

# Full model
mod1 <- lm(sales~pricegap+P2+P3+P4+P5+P6+P7,data=datL)
summary(mod1)
datgraf$sales <- predict(mod1,datgraf)
plot(datgraf$pricegap,datgraf$sales,type='l')
points(datL$pricegap,datL$sales,col='red')
points(datT$pricegap,datT$sales,col='green')

# LASSO model
n=10;p=9
XL <- as.matrix(datL[,!names(datL)=="sales"])
mod2 <- glmnet(XL, datL$sales, family="gaussian"
               ,alpha=1,nlambda=100,lambda.min.ratio=ifelse(n<p,0.01,0.0001)
               ,standardize=T,intercept=T
               ,type.gaussian="covariance"  #"naive" for n>500
               ,standardize.response=F
               ,type.multinomial="grouped")
plot(mod2,xvar="lambda")  #|"norm"|"dev"
print(mod2)
betanorm = colSums(abs(mod2$beta))
print(cbind(mod2$lambda,mod2$df,mod2$dev.ratio,betanorm))
XT <- as.matrix(datT[,!names(datT)=="sales"])
beta = predict(mod2, XT, s=exp(-3), type="coefficients");beta  #s=tau
salesT = predict(mod2, XT, s=exp(-3), type="response");salesT
# Reduced model
mod3 <- lm(sales~pricegap+P2+P3,data=datL)
summary(mod3)
datgraf$sales <- predict(mod3,datgraf)
plot(datgraf$pricegap,datgraf$sales,type='l')
points(datL$pricegap,datL$sales,col='red')
points(datT$pricegap,datT$sales,col='green')

# Basis Functions - Splines
require(splines)
mod4 = smooth.spline(x=datL$pricegap,y=datL$sales,df=5)
datgraf$sales <- predict(mod4,datgraf$pricegap)$y
plot(datgraf$pricegap,datgraf$sales,type='l')
points(datL$pricegap,datL$sales,col='red')
points(datT$pricegap,datT$sales,col='green')


