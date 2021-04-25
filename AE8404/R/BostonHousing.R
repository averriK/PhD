library(mlbench)
library(GGally)
data("BostonHousing")

DATA <- BostonHousing[,c("medv","nox","rm","lstat")]
ggcorr(DATA)
ggpairs(DATA)

MODEL <- lm(data= DATA, formula = medv ~ nox + rm + lstat)
summary(MODEL)
# Mirar correlacion con Y. La mejor es lstat maximo R2
# Mirar si las variables estan autocorrelacionadas
