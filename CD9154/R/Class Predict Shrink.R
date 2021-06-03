# Prediction-Continuous: Shrink
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


# Shrink: LASSO, Ridge, Elastic Net
library(glmnet)
n=nrow(XL);p=ncol(XL)
alpha=1   #LASSO:1, Ridge:0, ElasticNet:0-1


#Run model
mods <- glmnet(XL, yL, family="gaussian" #nlambda: numero de valores de tau a probar
               ,alpha=alpha,nlambda=100,lambda.min.ratio=ifelse(n<p,0.01,0.0001)
               ,standardize=T
               ,intercept=T #estima un beta0 para poder corregir la Y
               ,type.gaussian="covariance"
               ,standardize.response=F
               ,type.multinomial="grouped")
plot(mods,xvar="lambda")  #|"norm"|"dev"
print(mods)
betanorm = colSums(abs(mods$beta))
print(cbind(mods$lambda,mods$df,mods$dev.ratio,betanorm))
lambda = 4.34
# este valor de 4.34 viene de la línea modcv$lambda.min; log(modcv$lambda.min)

beta <- predict(mods,XT,s=lambda,type="coefficients")
beta <- as.numeric(beta)
names(beta) <- c("beta0",colnames(XT))
#write.table(beta,file="beta.csv",sep=",")
yps = predict(mods,XT,s=lambda,type="response");cor(cbind(yT,yps))^2;sqrt(crossprod(yT-yps)/nT)

#Unrestricted model with selected variables
beta <- beta[abs(beta[])>0.000001]
XLr <- XL[,colnames(XL) %in% names(beta)]
XTr <- XT[,colnames(XT) %in% names(beta)]
mod <- lm(yL~.,data=as.data.frame(cbind(yL,XLr)))
summary(mod)
write.table(mod$coef,file="beta.csv",sep=",")
yp = predict(mod,as.data.frame(XT));cor(cbind(yT,yp))^2;sqrt(crossprod(yT-yp)/nT)
det(cor(model.matrix(mod)[,-1]))


#Select lambda via Cross Validation
modcv <- cv.glmnet(XL, yL, family="gaussian"
                   ,alpha=alpha,nlambda=100,lambda.min.ratio=ifelse(n<p,0.01,0.0001)
                   ,standardize=T,intercept=T
                   ,type.gaussian="covariance"
                   ,standardize.response=F
                    ,type.multinomial="grouped" #util para variables discretas. Evita quitar parcialmente variables indicadoras (Estado) y las saca o mete de a grupos
                   ,type.measure="deviance"
                   ,nfolds=5
                   ,parallel=F)
modcv$lambda.min;
log(modcv$lambda.min)
print(cbind(modcv$lambda,modcv$nzero))
plot(modcv)
ypcv <- predict(modcv, XT, s="lambda.min");cor(cbind(yT,ypcv))^2;sqrt(crossprod(yT-ypcv)/nT)

