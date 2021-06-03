# Classification: Gaussian Mixtures
library(readxl)
library(mclust)
# setwd("C:/")
.setLocalWD()
rm(list=ls())   #clean memory

# read data
# data=read.table("Clasif02 Piel.txt",header=TRUE, sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
data <- read_excel("Clasif02 Piel enunciado.xlsx")
# select base variables
dataseg <- data[,1:11]


# evaluate options
gaumixbic <-mclustBIC(dataseg)
plot(gaumixbic)
summary(gaumixbic)

# estimate model
gaumix <- Mclust(dataseg,G=4,model="VEI")
summary(gaumix,parameters=T)

# append cluster assignment
seg <- map(gaumix$z)
data <- data.frame(data,seg)
write.table(file="Clasif02 Piel GaussMix.csv",aggregate(data,by=list(seg),FUN=mean),sep=",",dec=".")


