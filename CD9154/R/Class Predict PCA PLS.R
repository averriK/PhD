# Prediction-Continuous: PCA/PLS based
rm(list=ls())   #clean memory
.setLocalWD()
library(readxl)   # read Excel files



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

X <- model.matrix(lm(y~.-1,data=dat))
y <- model.frame(lm(y~.-1,data=dat))$y

n=nrow(X);p=ncol(X)

# Split sample: Learn-Test
nL = floor(0.8*nrow(dat))
nT = n-nL
set.seed(777)
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
yp = predict(mod,as.data.frame(XT));cor(cbind(yT,yp))[1,2]^2;sqrt(crossprod(yT-yp)/nT)
det(cor(model.matrix(mod)[,-1]))
vif(mod)


# PC (Principal Components)
library(pls)
n=nrow(XL);p=ncol(XL)
modpc <- mvr(yL ~ ., data=as.data.frame(cbind(yL,XL))
             ,scale=T,center=T
             ,method="svdpc"
             #             ,ncomp=3)
             ,validation="CV",segments=5)  #CV defines ncomp

# reduciendo las componentes de 42 a 21 tenemos el 92% de la varianza
summary(modpc,what="training")

plot(explvar(modpc))
npc=30
# root mean square error of prediction
RMSEP(modpc,estimate="train",ncomp=1:npc)    #RootMeanSqErrPred
yppc <- predict(modpc,ncomp=npc,newdata=XT);cor(cbind(yppc,yT))^2;sqrt(crossprod(yT-yppc)/nT)
# el error dio menor que antes
loadings(modpc) #relacion que hay entre las variables originales y las componentes principales

# R2 del ajuste entre y e y sombrero
plot(modpc,ncomp=npc,asp=1,line=T)  #y vs ypred


plot(modpc,plottype="scores",comps=1:min(7,npc))

#  aquí están los coefcientes beta de las 42 variables originales pero eliminada la colinealidad
beta <- modpc$coefficients[,,p];write.table(beta,file="beta.csv",sep=",")

# PLS (Partial Least Squares)
n=nrow(XL);p=ncol(XL)
modpls <- mvr(yL ~ ., data=as.data.frame(cbind(yL,XL))
              ,scale=T,center=T
              ,method="kernelpls"
              #             ,ncomp=3)
              ,validation="CV",segments=5)  #CV defines ncomp

summary(modpls,what="training")
validationplot(modpc,val.type="RMSEP")
plot(explvar(modpls))
nplsc=30
RMSEP(modpc,estimate="train",ncomp=1:nplsc)    #RootMeanSqErrPred
yppls <- predict(modpls,ncomp=nplsc,newdata=XT)
cor(cbind(yppls,yT))^2;sqrt(crossprod(yT-yppls)/nT)
datT <- as.data.frame(cbind(yT,XT));colnames(datT)[1]<-'yL'
RMSEP(modpls,estimate="test",newdata=datT,ncomp=nplsc)
loadings(modpls)
plot(modpls,ncomp=nplsc,asp=1,line=T)  #y vs ypred
plot(modpls,plottype="scores", comps=1:min(7,nplsc))
beta <- modpls$coefficients[,,p];
write.table(beta,file="beta.csv",sep=",")
