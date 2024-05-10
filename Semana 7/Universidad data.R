

#Declaracion de libreria utilizada
library(readxl)


#Insertar data mediante direccion
universidad <- read_excel("C:/Users/marco/OneDrive - Universidad Peruana Unión/Escritorio/7 mo ciclo/Analisis multiariado/GIT/Semana 7/universidad.xlsx")
View(universidad)

# Codigo GPT para que solo seleccione las columnas numéricas 🙏😎
numeric_cols <- sapply(universidad, is.numeric)
universidad_numeric <- universidad[, numeric_cols]
View(universidad_numeric)

#Matriz de correlacion
round(cor(x = universidad_numeric, method = "pearson"), 3)

#declarar libreria para graficos
library(psych)

#Se puede personalizar los colores a mostrar
multi.hist(x = universidad_numeric, dcol = c("orange", "red"), dlty = c("dotted", "solid"), main = "")


#Grafico 2
library(GGally)
ggpairs(universidad_numeric, lower = list(continuous ="smooth"), diag = list(continuous = "barDiag"), axisLabels ="none")

#IDENTIFICAMOS
modelox = lm(universidad$Founded_year ~ universidad$UK_rank+universidad$World_rank+universidad$score
             +universidad$Minimum_IELTS_score
             +universidad$fees
             +universidad$Student_satisfaction
             +universidad$Estimated_cost_of_living_per_year_(in_pounds)
             +universidad$Latitude)

#identificar variable que sirva para nuestro modelo

step(object = modelox, direction = "both", trace=1)
#Las variables que serviran son imc, grosor de piel y edad.

# paso 2
# Analizamos si existe colinealidad
library(car)
vif(modelox)

#paso 3 
# para ver el grafico
library(rgl)


#variables y x
plot3d(universidad$score , universidad$World_rank, universidad$Founded_year, pch = ".", size = 0.5)

#Fuin 👻