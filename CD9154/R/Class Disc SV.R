# Prediction-Discrete: Support Vector
rm(list=ls())
ROOT <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(ROOT)
library(readxl)   # read Excel files
library(e1071)
library(car)      #vif
library(compiler, lib.loc = "/usr/lib64/R/library")
# Functions
accu <- function(yp,yT) {ct=table(yp,yT);x=(ct[1,1]+ct[2,2])/sum(ct);return(paste("accu =",x))}
sens <- function(yp,yT) {ct=table(yp,yT);x=ct[2,2]/(ct[2,2]+ct[1,2]);return(paste("sens =",x))}
specif <- function(yp,yT) {ct=table(yp,yT);x=ct[1,1]/(ct[1,1]+ct[2,1]);return(paste("specif =",x))}

# read data
dat <- read_excel("Discrim01 spam.xlsx")

# Split sample: Learn-Test
n = floor(0.8*nrow(dat))
set.seed(777)
Lind = sample(seq_len(nrow(dat)),size = n)
datL = dat[Lind,]
datT = dat[-Lind,]
xL = datL[,!(names(datL)=="y")]
yL = as.factor(datL$y)
xT = datT[,!(names(datT)=="y")]
yT = as.factor(datT$y) #vector de factores. En general en todos los modelos de discriminación se adopta este criterio. dos categorias o valores

# Support Vector
n=nrow(xL);p=ncol(xL)
XL = as.matrix(xL)
XT = as.matrix(xT)
# XT <- model.matrix(lm(y~.,dat)) # si existieran variables categoricas, el lm codifica automaticamente


modsv <- svm(x=XL, y=yL # ingresando las variables x e y no requiere formula ni datos.
             ,type="C-classification"
             ,kernel="radial", cost=2 #C-Class only
             ,cachesize=400, tolerance=0.001 #no es nuestro epsilon
             ,epsilon=0.1
             ,shrinking=T # tiende a simplificar y regularizar
             ,cross=0 # validacion cruzada. conviene usarlo sobre datos frescos para definir los hiperparametros
             ,probability=T #reportar la probabilidad de que un punto esté en un lado u otro
             ,fitted=T
             )
# C-classification: Predicción Discreta- Tal como vimos en la teoría
# Una forma de abordar el hyperparameter. parametro c (llamado "cost") en el modelo que regula el ancho de banda

length(modsv$index) #995 vectores de soporte. Los puntos de soporte dividen

dim(modsv$SV) #1080 vectores de 57 variables.
yp = predict(modsv, XT, probability=F) #Probabilidad = FALSE porque quiern ver 1 y 0
ct = table(yp,yT);ct;accu(yp,yT);sens(yp,yT);specif(yp,yT)
(ct)

# nu-classification Predicción Discreta.
# Otra forma de abordar el hyperparameter
#
modsv <- svm(x=XL, y=yL # ingresando las variables x e y no requiere formula ni datos.
             ,type="nu-classification"
             ,kernel="radial", nu=0.3 #nu-Class only
             ,cachesize=400, tolerance=0.001 #no es nuestro epsilon
             ,epsilon=0.1
             ,shrinking=T # tiende a simplificar y regularizar
             ,cross=0 # validacion cruzada. conviene usarlo sobre datos frescos para definir los hiperparametros
             ,probability=T #reportar la probabilidad de que un punto esté en un lado u otro
             ,fitted=T
)
# con nu-class, controlo la cantidad de puntos de soporte. con C-class controlo el ancho de la banda, los epsilon
# el metodo no puede predecir multiclass-classif (que es cuando la variable Y no es binaria)
# y para salvar esto, ver help

length(modsv$index) #995 vectores de soporte. Los puntos de soporte dividen

dim(modsv$SV) #1080 vectores de 57 variables.
yp = predict(modsv, XT, probability=F) #Probabilidad = FALSE porque quiern ver 1 y 0
ct = table(yp,yT);ct;accu(yp,yT);sens(yp,yT);specif(yp,yT)
(ct)



# one-classification (for novelty detection)
# Predicción Discreta
#
# eps-regression
# Predicción Continua
#
# nu-regression
# Predicción Continua

