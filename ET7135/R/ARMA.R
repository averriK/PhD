library(forecast)
library(urca)
library(readr)
library(readxl)
library(data.table)
library(magrittr)
setwd("~/git/PhD/ET7135/R")
dat <- read_csv("SC36_BJ_F_Yield.txt") %>% as.data.table()
#dat<-read.table(file="SC36 B&J F Yield.txt",sep="\t",dec=".",header=T)
#dat<-read.table(file="SC36 B&J F Yield.csv",sep=";",dec=",",header=T)

# Convertir el dataframe en un objeto tipo Time-Series
y = ts(data=dat[,1],start=1,frequency=1)

# Graficar la serie (chequear homocedasticidad y trends)
plot(y)

# Centrado de la serie estacionaria (necesario para los problemas con media variable)
u = y - mean(y)

# Autocorrelacion function
# (siempre el primer elemento k=0 es acf=1)
# la linea punteada muestra el test de Battle
# el elemento 2 es negativo. Tipico de procesos industriales autocorrelaciones negativas, donde actuó un sistema de control 
# hasta el k=2 son significativos.
acf(u)

# Para un AR el decaimiento de la funcion de acf es gradual. Aqui no es claro y deberíamos probar
# siendo k>2 despreciable, MA(2) es un candidato. MA(3) podria ser pero tiene poca chance, porque acf(3) esta cerca de la banda de la sigificancia
# Otro candidato podria ser un ARMA(1,1)


# ******************************************************************
# mod <- arima(u,order=c(0,0,2))  # MA(2)   
# Modelo MA(2) El intercept chico refleja que centramos la serie. Si no la centrabamos, el intercept seria el valor de la media
# Estacionalidad: No se require vealidar porque es un MA puro
# Hay que validar invertibilidad porque es MA, emdiante las raices de la ecuacion caracteristica


# ******************************************************************
# mod <- arima(u,order=c(1,0,0))  # AR(1)   
# Modelo AR(1)
# fi1 es negativo porque la primera autocorrelacion era negativa en el plot del acf

# ******************************************************************
mod <- arima(u,order=c(1,0,1))  # ARMA(1,1)   

mod

# Significancia parametros
param <- sprintf("%1.3f",mod$coef)
tstat <- mod$coef/sqrt(diag(mod$var.coef))
pvalue <- 2*(1-pnorm(abs(tstat))); 
pvalue <- sprintf("%1.3f",pvalue)
cbind(param,tstat,pvalue)
# AR(1): el p-value es 0. el valor de fi es estable. Se rechaza H0
# ARMA(1,1). el ma1 tiene algun tipo de colinealidad, pero no invalida el modelo
# Nota: R en los modelos de media movil los reporta positivos, pero hay que cambiar el signo para que corresponda con la teoria

# Raices ecuacion caracteristica
plot(mod)
# esta instruccion se viaualiza con plot(mod) las inversas (1/z)
# Modelo MA(2): Todos dentro. => Cumple invertibilidad
# se puede ver que ambas raíces son complejas pero mayores que 1 (1/z >1)
# si NO fuera invertible, veriamos que el modelo, expresado en su forma autroregresiva, mostraría una dependencia cada vez mayor con los valores más viejos
# AR(1): TODOS dentro del circulo. Cumple Estacionaridad. eN PRINCIPIO NO HACE FALTA CALCULAR RICES. SOlo ver que los coeficientes son en modulo menor que 1
# ARMA(1,1). Dos circulos complejos. Las reciprocas de las raíces son iguales a los parámetros

# Residuos
plot(mod$residuals/sqrt(mod$sigma2))
acf(mod$residuals)
# El objeto mod tiene los residuos como mod$residuals
# MA(2): ninguno cae fuera de las bandas de significancia. Eso es bueno
# AR(1): Todas las acf son chicas. Los evaluamos con L. Box
# El ARMA(1,1) funciona bien y era de esperarse porque ya el AR(1) filtraba bien y todo caia debajo de los limites y ahora es un AR(1) mas otro filtro

# Ljung & Box
# Segunda validacion: Ensayo sobre los residuos
npar <- 2 #ARMA=2
i<-1
H0<-NA
Q<-NA
df<-NA
pval<-NA
for (i in 1:17) {
  # Consulta: Creo que debería ser i = npar:10, ya que menos de npar no hay i (NaN)
  LB <- Box.test(mod$residuals,lag=i,type="Ljung-Box",fitdf=npar)
  # lag: maxima autocorrelacion considerada en la hipotesis H0
  # i=7: significa que la hipotesis H0: rho(1)=rho(2)=...=rho(7)=0
  # fitdf=npar : parametros que habia en el modelo que le introdujimos. era un MA(2), luego npar=2
  
  H0[i]=paste("r1=...=r",toString(i),"=0",sep="")
  Q[i]=LB$statistic
  df[i]=LB$parameter
  pval[i]=LB$p.value
  }
tab<-cbind(H0,Q,df,sprintf("%1.4f",pval))
colnames(tab)[4]<-"p-val"
print(tab,quote=F)
# Ljung & Box MA(2)
# MA(2): A partir de h=3 se observa que ninguno queda por debajo de 0.5. Significa que NO se rechaza la hipotesis
# Luego, NO hay autocorrelacion entre los residuos. Esto valida el modelo
# AR(1)- Ningun valor hasta h=17. Cumple. No tiene dinamica residual

# Schwarz
BIC(mod)
# MA(2): El modelo supero dos criterios de validez. Puedo calcular entonecs el BIC bic=547
# AR(1) BIC=544 < 547. El modelo AR(1) es mejor, aparentemente
# ARMA(1,1) es el que menor desvio estandard tiene, pero no le puede ganar al AR(1)



# Verificación de tendencia determinista
t = 1:length(y)
mlin <- lm(y~t)
summary(mlin)
plot(y,col='red')
lines(t,predict(mlin),col='blue')
# Eliminar tendencia
u = resid(mlin)
plot(u)

# Verificación de ciclos (Dickey & Fuller)
library(urca)
# H0) y(t) tiene raiz unitaria
summary(ur.df(y,type="trend",lags=24)) #D&F: type=none|drift|trend
u = diff(y)
plot(u)
summary(ur.df(u,type="none",lags=12))

# ************************************************************************************************
# Modelo ARIMA estacional

# Import MDin024 hyndman sales12.txt
y = ts(data=dat[,2],start=1,frequency=12)
plot(y)
plot(log(y))

t = 1:length(y)
mstat = lm(log(y)~t)
u = ts(residuals(mstat),start=1,frequency=12)
plot(u)

mod<-arima(y,order=c(1,0,0),seasonal=list(order=c(1,0,0),period=12))
mod
mod<-arima(y,order=c(1,0,1),seasonal=list(order=c(1,1,0),period=12))
mod
# validar y comparar



# Deflactar
library(readxl)
cpiusa <- read_xls("C:/Eop/@Acad/@Material/MDinamico/CPI USA.xls")
cpi = cpiusa$cpi
cpi = cpi/cpi[length(cpi)]
y = y / cpi
plot(y)

