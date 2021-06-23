#### ACTIVIDAD 3: Cluster Vinos
library(readxl)

# 1. Cargue el archivo winequqlity-red.xlsx
winequality_red <- 
  read_excel("D:/@acad_topics/ma_cluster/Casos/winequality/winequality/winequality-red.xlsx")

# 1 - fixed acidity
# 2 - volatile acidity
# 3 - citric acid
# 4 - residual sugar
# 5 - chlorides
# 6 - free sulfur dioxide
# 7 - total sulfur dioxide
# 8 - density
# 9 - pH
# 10 - sulphates
# 11 - alcohol
# 12 - quality (score between 0 and 10)

# 2. Grafique los dendogramas para los siguinetes casos. 
#     Solo utilice las vaariables 1 a 11
# a. Con las variables en el estado original
# b. Con las variables estandarizadas.
#
# Compare los dendogramas, ¿cual utilizaría?

# Para estandarizar las variables
dataclu_std <- winequality_red[, 1:11]
for (row in 1:11){
  dataclu_std[, row] <- scale(dataclu_std[, row])
}
  
# 3. Genere 3 cluster con hclust
# a. Con las variables en el estado original
# b. Con las variables estandarizadas.
#
# Calcule las medias de cada una de las variables en cada uno de los cluters en cada uno de los casos. Puede describir los clusters con esta informacion.


# 4. Utilice el método de Kmeans para determinar las cantidad optima de clusters. 
#    Genere los clusters óptimos. Interprete estos grupos.
   

# 5. Utilice el método de Gaussian Mixtures, seleccione 
#    el método y la cantidad optima de clusters segun BIC.