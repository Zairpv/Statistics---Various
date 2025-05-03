#Buscar directorio
setwd("C:/a_a_Ana_Karen/Cursos_Otoño_2022/ANOVA_MR")
dir()

#Cargar paquetes
library(agricolae)
library (tidyverse)
library (ggpubr)
library (rstatix)
library (datarium)

# Leer datos
datos<-read.csv(file="completa.csv",header=TRUE)

# Para obtener la estructura de la base de datos
str(datos)

# Unique es la función para obtener los "factores" o variables que se repiten
unique(datos$mes)

# los meses únicos
meses <- unique(datos$mes)
class(meses)

# Vector de meses en factor
meses_ordenados <- factor(datos$mes, labels = meses)
class(meses_ordenados)
levels(meses_ordenados)

# Cambiar de character to factor (columna = mes)
datos$mes <- factor(datos$mes, labels = meses)

# Boxplot
boxplot(flujo ~ mes, data = datos)

#Histograma de flujo vs edades



# boxplot flujo vs meses
library(ggplot2)
ggplot(data = datos, aes(x = mes, y = flujo)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 17)) 

# boxplot flujo vs edad
library(ggplot2)
ggplot(data = datos, aes(x = as.factor(edad), y = flujo)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = "Edad",
       y = bquote("Flujo de"~CO[2]))

help(package = "ggplot2")


# Convertir edad a factor
datos$edad <- factor(datos$edad)

# Análisis de Varianza
anova_tres_factores <- aov(flujo ~ edad + mes + edad*mes,
                           data = datos)
summary(anova_tres_factores)

# Histograma de frecuencias
hist(datos$flujo)

# Boxplot
boxplot(datos$flujo, ylim = c(0,15))

# Prueba de normalidad de Shapiro
shapiro.test(datos$flujo)

