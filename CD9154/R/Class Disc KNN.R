# Prediction-Discrete: K Nearest Neighbour
ROOT <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(ROOT)
library(readxl)   # read Excel files
library(class)    #KNN
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
datL = dat[Lind,] # datL: Datos de Aprendizaje
datT = dat[-Lind,] # datL: Datos de Prueba
xL = datL[,!(names(datL)=="y")]
yL = as.factor(datL$y)
xT = datT[,!(names(datT)=="y")]
yT = as.factor(datT$y)

# KNN class
k=1
yp = knn(xL, xT, yL, k=k, l=0, prob=F, use.all=T)
ct = table(yp,yT);ct;accu(yp,yT);sens(yp,yT);specif(yp,yT)
mean(dat$y) # 39% de Spam ("morosos). mucho más balanceada
ACC <- vector()
SEN <- vector()
SPE <- vector()
for(k in 1:20){
  yp = knn(xL, xT, yL, k=k, l=0, prob=F, use.all=T)
  ct = table(yp,yT);
  ACC[k] <- accu(yp,yT)
  SEN[k] <- sens(yp,yT)
  SPE[k] <- specif(yp,yT)
}

