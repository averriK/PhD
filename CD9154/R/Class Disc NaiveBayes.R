# Prediction-Discrete: K Nearest Neighbour
ROOT <- dirname(rstudioapi::getSourceEditorContext()$path)
# Prediction-Discrete: Na?ve Bayes

library(readxl)   # read Excel files
library(e1071)
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

# Na?ve Bayes
yp = predict(naiveBayes(xL,yL),xT)
ct = table(yp,yT);ct;accu(yp,yT);sens(yp,yT);specif(yp,yT)

