library(mlbench)
library(GGally)
data("mtcars")

DATA <- mtcars[,c("mpg","disp","wt","cyl","vs")]

ggpairs(data = DATA)
# Calcular para cada punto del diseño, el IC del 95% para la respuesta
MODEL <- lm(data= DATA, formula = mpg ~ disp + wt + cyl+vs)
X <- model.matrix(MODEL)
A <- solve(t(X)%*%X)
summary(MODEL)
predict(MODEL,)
ggplot(data=DATA) +
  geom_point(mapping = aes(x=wt, y=mpg),colour="blue",size=2)
y <- mtcars$mpg
xo <- mtcars$wt
# Yo sombrero
Yo <- MODEL$fitted.values
t.test(xo, conf.level = .95)
pt(DATA$wt,n=n,df=n-p)
n <- length(xo)

X <- as.matrix(data.frame(I=rep(1,n), X=xo))
B <- solve(t(X) %*% X) %*% t(X) %*% y

X <- as.matrix(data.frame(I=rep(1,n), X2=x)


p <- 1
df <- n-p
a=5/100
