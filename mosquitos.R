# abrimos el archivo mosquitos.txt
datos <- read.table ("D:/Desktop/mosquitos/mosquitos.txt", header = TRUE)

#Y= abund
#X1= verdor
#X2= humedad
#X3= temperatura

# exploración de los datos
layout(matrix(1:4,2,2))
plot(datos$abund);
plot(datos$verdor);
plot(datos$humedad);
plot(datos$temperatura); 
layout(1)

# exploración bivariada de los datos
pairs(datos)

# Detección de la colinealidad
library(car)

# A) Gráficos SPLOM
scatterplotMatrix(~ verdor + humedad + temperatura,
                  smooth=TRUE,  
                  diagonal = 'density', 
                  data = datos) 

# B) Matrices de correlación
CORR <- cor(datos[, c("verdor","humedad", "temperatura")]) 
CORR
# segun el criterio de correlacion y colinealidad (r>0,7) no deberiamos quitar ninguna variable

# C) Factores de inflación de la varianza
fit <- lm(abund ~ verdor + humedad + temperatura, data = datos) 
vif(fit) 
#segun el criterio de vif > 5 no debemos quitar ninguna variable

#veo que tengo un modelo sin colinealidad, asi que paso a analizar sus supuestos
layout(matrix(1:4, 2, 2))
plot(fit)
layout(1)
#los datos cumplen los supuestos
#el modelo es normal, y lineal

##############################################################################

# Construccion y Selección de modelos posibles
library(leaps)
#hacemos una busqueda exhaustiva
BS.fit <- regsubsets(abund ~ verdor + humedad + temperatura, 
                     data = datos, 
                     method = "exhaustive")
BS.summary <- summary(BS.fit)
BS.summary 

#elijo el criterio con el que voy a seleccioar la cantidad de variables del modelo optimo
#el mejor modelo es el menos variable y el menos sesgado
plot(BS.summary$cp, xlab = "número de variables", ylab = "Cp", type ="l") 
plot(BS.fit, scale = "Cp")
plot(BS.summary$bic, xlab = "número de variables", ylab = "BIC", type ="l")
plot(BS.fit, scale = "bic")
#según Cp= el mejor modelo tiene 1 variable, y es= verdor
#según BIC= el mejor modelo tiene 1 variable, y es= verdor

#elegimos el modelo más predictivo, el que tiene menos variables
#seguimos el criterio de Cp y Bic ya que coinciden
coef(BS.fit, 1)

#Pruebo los supuestos para el modelo seleccionado con variables obtenido con
fit2 <- lm(abund ~ verdor, data = datos)
layout(matrix(1:4, 2, 2))
plot(fit2)
layout(1)

#gráfico utilizando ggplot
library(ggplot2)

ggplot(data = datos, aes(x = verdor, y = abund)) + geom_point()    

g1 <- ggplot(data = datos, aes(x = verdor, y = abund))  
g2 <- g1 + geom_point(size=1, color="red") 
g3 <- g2 + xlab("Verdor")  
g4 <- g3 + ylab("Abundancia")
g5 <- g4 + theme_bw()
g6 <- g5 + geom_smooth(method = "lm")
g6