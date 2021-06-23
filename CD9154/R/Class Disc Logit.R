# Prediction-Discrete: Logistic & Shrinking
ROOT <- dirname(rstudioapi::getSourceEditorContext()$path)
rm(list=ls())
library(readxl)   # read Excel files
library(glmnet)   #Logit, LASSO, Elastic net
library(car)      #vif

# Functions
accu <- function(yp,yT) {ct=table(yp,yT);x=(ct[1,1]+ct[2,2])/sum(ct);return(paste("accu =",x))}
sens <- function(yp,yT) {ct=table(yp,yT);x=ct[2,2]/(ct[2,2]+ct[1,2]);return(paste("sens =",x))}
specif <- function(yp,yT) {ct=table(yp,yT);x=ct[1,1]/(ct[1,1]+ct[2,1]);return(paste("specif =",x))}

# read data
#rm(list=ls())
dat <- read_excel("Discrim01 spam.xlsx")

# Split sample: Learn-Test
n = floor(0.8*nrow(dat))
set.seed(001)
Lind = sample(seq_len(nrow(dat)),size = n) 
datL = dat[Lind,]
datT = dat[-Lind,]
xL = datL[,!(names(datL)=="y")]
yL = as.factor(datL$y)
xT = datT[,!(names(datT)=="y")]
yT = as.factor(datT$y)

# Logit
modlg <- glm(y~.,binomial(link="logit"),data=datL) # datL: Datos de Aprendizaje
summary(modlg)
proba = predict(modlg,xT,type="response")
cut = 0.4
yp=NULL;yp[proba>=cut]=1;yp[proba<cut]=0
ct = table(yp,yT);ct;accu(yp,yT);sens(yp,yT);specif(yp,yT)

# Logit - Shrink
n=nrow(xL);p=ncol(xL)
XL = as.matrix(xL);XT = as.matrix(xT)
modlgs <- glmnet(XL, yL, family="binomial"
                 ,alpha=1,nlambda=100,lambda.min.ratio=ifelse(n<p,0.01,0.0001)
                 ,standardize=T,intercept=T
                 ,type.logistic="Newton"
                 ,standardize.response=F
                 ,type.multinomial="grouped")
plot(modlgs)
plot(modlgs,xvar="lambda")
betanorm = colSums(abs(modlgs$beta))
print(cbind(modlgs$lambda,modlgs$df,modlgs$dev.ratio,betanorm))
beta = predict(modlgs, XT, s=0.00452, type="coefficients");beta
proba = predict(modlgs, XT, s=0.00452, type="response")
yp = predict(modlgs, XT, s=0.00452, type="class")
ct = table(yp,yT);ct;accu(yp,yT);sens(yp,yT);specif(yp,yT)

# Cross Validation for glmnet
modlgscv <- cv.glmnet(XL, yL, family="binomial"
                      ,alpha=1,nlambda=100,lambda.min.ratio=ifelse(n<p,0.01,0.0001)
                      ,standardize=T,intercept=T
                      ,type.logistic="Newton"
                      ,standardize.response=F
                      ,type.multinomial="grouped"
                      ,type.measure="class" # lo que indica el plot
                      ,nfolds=5
                      ,parallel=T)
plot(modlgscv)
modlgscv$lambda.min
print(cbind(modlgscv$lambda,modlgscv$nzero))
yp = predict(modlgscv, XT, s="lambda.min", type="class")
ct = table(yp,yT);ct;accu(yp,yT);sens(yp,yT);specif(yp,yT) # me quedo con el metodo anterior.
 # tau optimo
# log(modlgscv$lambda.min)
