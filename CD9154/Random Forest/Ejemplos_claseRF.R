library(readxl)
library(readr)
library(ggplot2)
library(rpart)
library(rattle)
library(dplyr)
library(randomForest)
Pred004_King_House_Price <- 
  read_excel("data/Pred004 King House Price.xlsx")
Discrim01_spam <- 
  read_excel("data/Discrim01 spam.xlsx")
airbnb <-
  read_csv("data/airbnb.csv")

# Cleaning
library(stringr)
airbnb$price <- 
  str_replace_all(string = airbnb$price, 
                  pattern =  "\\$",
                  replacement =  "")
airbnb$price <- 
  str_replace_all(string = airbnb$price, 
                  pattern =  ".00",
                  replacement =  "")
airbnb$price <- 
  str_replace_all(string = airbnb$price, 
                  pattern =  ",",
                  replacement =  "")
airbnb$price <- as.numeric(airbnb$price)
airbnb <- airbnb %>% 
  filter(price >= 30)



arbol <-
  rpart(formula = price ~ sqft_above,
        data = Pred004_King_House_Price)
fancyRpartPlot(arbol)

arbol <-
  rpart(formula = price ~ sqft_above,
        control = rpart.control(
          minsplit = 20, 
          minbucket = round(20/3), 
          cp = 0.01,
          maxcompete = 4, 
          maxsurrogate = 5, 
          usesurrogate = 2, 
          xval = 10,
          surrogatestyle = 0, 
          maxdepth = 1),  # CANTIDAD MAXIMA DE NIVELES DE PROFUNDIDAD
        data = Pred004_King_House_Price)
# rpart.plot(arbol)
fancyRpartPlot(arbol)
arbol

ggplot(data = Pred004_King_House_Price)+
  geom_point(aes(x= sqft_above, y = price))+
  geom_vline(xintercept = 2829, color = 'red', size = 2, alpha = .4)


arbol <-
  rpart(formula = price ~ zipcode,
        data = Pred004_King_House_Price)
fancyRpartPlot(arbol)

arbol <-
  rpart(formula = price ~ factor(zipcode),
        data = Pred004_King_House_Price)
fancyRpartPlot(arbol)
arbol


ggplot(data = airbnb)+
  geom_boxplot(aes(x= room_type, y = log(price)))
arbol <-
  rpart(formula = log(price) ~ room_type,
        data = airbnb)
fancyRpartPlot(arbol)




arbol <-
  rpart(formula = factor(y) ~ .,
        control = rpart.control(
          minsplit = 20, 
          minbucket = round(20/3), 
          cp = 0.01,
          maxcompete = 4, 
          maxsurrogate = 5, 
          usesurrogate = 2, 
          xval = 10,
          surrogatestyle = 0, 
          maxdepth = 10),
        data = Discrim01_spam)
# rpart.plot(arbol)
fancyRpartPlot(arbol)


library(randomForest)
rf <- randomForest(price ~ . -zipcode, 
                   data = Pred004_King_House_Price)

Pred004_King_House_Price$zipcode <- 
  factor(Pred004_King_House_Price$zipcode)

rf <-
  randomForest(
    formula = price ~ . -id -date -year -zipcode,
    data = Pred004_King_House_Price[1:2000,])

rf
summary(rf)

rf <-
  randomForest(
    formula = price ~ . -id -date -year -zipcode,
    data = Pred004_King_House_Price[1:2000,],
    ntree=100)

df <- data.frame()
for (tree in 100:105){
  rf <- randomForest(
    formula = price ~ . -id -date -year -zipcode,
    data = Pred004_King_House_Price[1:2000,],
    ntree=tree)
  df <- bind_rows(
    df,
    data.frame(
      ntree = tree,
      mse = mean(rf$mse),
      rsq = mean(rf$rsq))
    )
  print(tree)
}


rf <-
  randomForest(
    formula = factor(y) ~ . ,
    data = Discrim01_spam,
    ntree=100)