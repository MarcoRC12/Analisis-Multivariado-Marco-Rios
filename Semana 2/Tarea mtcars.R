install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
library(corrplot)
library(ggplot2)
#CUANTAS FILAS Y COLUMNAS TIENEN
dim(mtcars)

#TIPO DE DATOS , double, string
str(mtcars)

#//Nombres tienen las variables
names(mtcars)

#//Datos descriptivos de las variables
summary(mtcars)

# Ejecutar matriz de correlaciones
M = cor(mtcars)
corrplot(M, method = "ellipse")
corrplot(M, method = "circle")
corrplot(M, method = "square")
corrplot(M, method = "number")
corrplot(M, method = "shade")
corrplot(M, method = "color")
corrplot(M, method = "pie")






