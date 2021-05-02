library(forecast)
library(urca)
library(readr)
library(readxl)
library(data.table)
library(magrittr)
setwd("~/git/PhD/ET7135/R")
dat <- read_csv("SC22_T-bills.txt") %>% as.data.table()

# Verificar si es estacionario, verificar si no tiene tendencias
y = ts(data=dat[,1],start=1,frequency=1)
plot(y)
u = y - mean(y)
plot(u)
# Autocorrelacion function
acf(u)

# El autocorrelarograma baja de a poco. lo que indica un tipo de tendencia

