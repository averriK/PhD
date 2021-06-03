# Prediction-Continuous: Sequential Model Search

# setwd("C:/")
.setLocalWD()
library(readxl)   # read Excel files
library(car)
rm(list=ls())   #clean memory

# read data
dat <- read_excel("Pred004 King House Price.xlsx")
dat <- subset(dat,select=c(price,bedrooms,bathrooms,sqft_living,sqft_lot,sqft_living15,sqft_lot15,floors,waterfront,view,condition,grade,bsmt,sqft_bsmt,age,renov,age_renov))
colnames(dat) <- c("y","x01","x02","x03","x04","x05","x06","x07","x08","x09","x10","x11","x12","x13","x14","x15","x16")

# data preparation
dat$y <- dat$y/1000
dat$x03 <- dat$x03*(0.3048^2)
dat$x04 <- dat$x04*(0.3048^2)
dat$x05 <- dat$x05*(0.3048^2)
dat$x06 <- dat$x06*(0.3048^2)
dat$x13 <- dat$x13*(0.3048^2)
dat$x01.2 <- dat$x01^2;dat$x01.3 <- dat$x01^3
dat$x02.2 <- dat$x02^2;dat$x02.3 <- dat$x02^3
dat$x03.2 <- (dat$x03-mean(dat$x03))^2;dat$x03.3 <- (dat$x03-mean(dat$x03))^3
dat$x04.2 <- (dat$x04-mean(dat$x04))^2;dat$x04.3 <- (dat$x04-mean(dat$x04))^3
dat$x05.2 <- (dat$x05-mean(dat$x05))^2;dat$x05.3 <- (dat$x05-mean(dat$x05))^3
dat$x06.2 <- (dat$x06-mean(dat$x06))^2;dat$x06.3 <- (dat$x06-mean(dat$x06))^3
dat$x07.2 <- (dat$x07-mean(dat$x07))^2;dat$x07.3 <- (dat$x07-mean(dat$x07))^3
dat$x09.2 <- (dat$x09-mean(dat$x09))^2;dat$x09.3 <- (dat$x09-mean(dat$x09))^3
dat$x10.2 <- (dat$x10-mean(dat$x10))^2;dat$x10.3 <- (dat$x10-mean(dat$x10))^3
dat$x11.2 <- (dat$x11-mean(dat$x11))^2;dat$x11.3 <- (dat$x11-mean(dat$x11))^3
dat$x13.2 <- (dat$x13-mean(dat$x13))^2;dat$x13.3 <- (dat$x13-mean(dat$x13))^3
dat$x14.2 <- (dat$x14-mean(dat$x14))^2;dat$x14.3 <- (dat$x14-mean(dat$x14))^3
dat$x16.2 <- (dat$x16-mean(dat$x16))^2;dat$x16.3 <- (dat$x16-mean(dat$x16))^3

# obligamos a hacer una regresion que no necesitamos, para sacar la matriz X codificada
# en nuestro caso no es necesario codificar pero es mejor hacer este paso siempre
X <- model.matrix(lm(y~.-1,data=dat))
y <- model.frame(lm(y~.-1,data=dat))$y

n=nrow(X);p=ncol(X)

# Split sample: Learn-Test
nL = floor(0.8*nrow(dat)) #learning
nT = n-nL # testing
set.seed(777) # reproductividad de los resultados

Lind = sample(seq_len(nrow(dat)),size=nL)
XL = X[Lind,]
yL = y[Lind]
XT = X[-Lind,]
yT = y[-Lind]
datL = cbind(yL,XL);colnames(datL)[1]="y"


# Linear Simple
mod01 <- lm(yL~x01+x02+x03+x04+x05+x06+x07+x08+x09+x10+x11+x12+x13+x14+x15+x16
            ,data=as.data.frame(cbind(yL,XL)))
mod <- mod01
summary(mod)
#para calcular el desempeño vamos a calcular el R2 sobre la muestra de prueba
yp = predict(mod,as.data.frame(XT));cor(cbind(yT,yp))[1,2]^2;sqrt(crossprod(yT-yp)/nT)
det(cor(model.matrix(mod)[,-1]))
# los vif's altos indican colinealidad
vif(mod)

## ----------------------------------------------
# Sequential Model Search
library(leaps)
# metodo forward. Martrices de learning
modfw <- regsubsets(x=XL,y=yL,nvmax=10,method="forward",really.big=F) # forward
plot(modfw) #el modelo presenta un plot del BIC de cada variable
mod02 <- lm(yL ~ x02+x03+x07+x08+x09+x11+x14+x02.2+x03.2+x07.3+x09.2+x11.2+x11.3
            ,data=as.data.frame(cbind(yL,XL)))
mod <- mod02

modbw = regsubsets(x=XL,y=yL,nvmax=10,method="backward",really.big=F)
plot(modbw)
mod03 <- lm(yL ~ x02+x03+x08+x09+x11+x14+x03.2+x09.3+x11.2+x14.2+x14.3
            ,data=as.data.frame(cbind(yL,XL)))
mod <- mod03

summary(mod)
yp = predict(mod,as.data.frame(XT));cor(cbind(yT,yp))^2;sqrt(crossprod(yT-yp)/nT)
det(cor(model.matrix(mod)[,-1]))




