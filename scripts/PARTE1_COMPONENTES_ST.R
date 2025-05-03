##1. Conjunto de datos 
rm(list=ls()) 
options(max.print = 1000000) 
setwd("D:/zaira/Documents/08. R scripts/Series - Precios cafe")

##2. Datos
datos_anual<-read.csv("Cafe_anual.csv",header=TRUE) 
datos_anual

##3.Librerias 
library(forecast) 
library(tseries) 
library(ggfortify) 
library(TTR) 
library(moments)

##4. Creacion de la serie
names(datos_anual)
attach(datos_anual)

#Variable: cierre
ts_cierre=ts(cierre,frequency = 1,start = 1987,end = 2022,names = "Cierre")
ts_cierre
ts_apertura=ts(apertura,frequency = 1,start = 1987,end = 2022,names = "Apertura")
ts_apertura
ts_maximo=ts(maximo,frequency = 1,start = 1987,end = 2022,names = "Maximo")
ts_maximo
ts_minimo=ts(minimo,frequency = 1,start = 1987,end = 2022,names = "Minimo")
ts_minimo






##5. Estadistica descriptiva de la serie
par(mfrow=c(2,2),cex=0.9) 
hist(ts_cierre,main="a)",ylab="Frecuencia",xlab="Precio de cierre") 
hist(ts_apertura,main = "b)",ylab = "Frecuencia",xlab="Precio de apertura")
hist(ts_maximo,main = "c)",ylab = "Frecuencia",xlab="Precio máximo")
hist(ts_minimo,main = "d)",ylab = "Frecuencia",xlab="Precio minimo")
dev.off()

##5. Estadistica descriptiva de la serie
#Nota Evaluar para cada variable 
summary(ts_cierre)
var(ts_cierre,na.rm = TRUE)
sd(ts_cierre,na.rm = TRUE)
kurtosis(ts_cierre,na.rm = TRUE)
skewness(ts_cierre,na.rm = TRUE)

##6. Gráfica de densidad de la serie #Para evaluar la distribución de la serie
par(mfrow=c(2,2),cex=0.9)
plot(density(ts_cierre,na.rm = TRUE), main = "a)") 
plot(density(ts_apertura,na.rm = TRUE), main = "b)") 
plot(density(ts_maximo,na.rm = TRUE), main = "c)") 
plot(density(ts_minimo,na.rm = TRUE), main = "d)") 
dev.off()

##7. Grafica de las series
plot.ts(ts_cierre)
plot.ts(ts_apertura)
plot.ts(ts_maximo)
plot.ts(ts_minimo)
#El patron es el mismo! en todas las variables
#aparentemente el patron que se repite cada año no es constante, por lo que posiblemente
#la serie no sea estacionaria

##8. Verificar estacionariedad
#En una serie estacionaria se espera una funcion de autocorrelación (ACf) que va a cero para 
#cada lag o cambio en el tiempo
acf(ts_cierre,na.action = na.pass)
autoplot(acf(ts_cierre,na.action = na.pass)) #otra forma de ver la grafica
#hay varios retrasos que superan los intervalos de confianza de ACF (linea azul) ----> No estacionariedad


##9. Descomposición de las series de tiempo
#Se necesita descomponer en sus constituyentes: tendencia, componente aleatorio y estacionalidad
#Suavizamiento por promedios moviles
ts_cierre_sma1=SMA(ts_cierre,n=1) #aqui hay q ir evaluando cual es el mejor valor de n para suavizamiento
autoplot(ts_cierre_sma1,ts.colour = "dark green",xlab = "Año",ylab = "Precio de cierre")

ts_cierre_sma2=SMA(ts_cierre,n=2)
autoplot(ts_cierre_sma2,ts.colour = "dark green",xlab = "Año",ylab = "Precio de cierre")

ts_cierre_sma3=SMA(ts_cierre,n=3) #Se pierde mucho un patron
autoplot(ts_cierre_sma3,ts.colour = "dark green",xlab = "Año",ylab = "Precio de cierre")

ts_cierre_componentes=decompose(ts_cierre)
plot(ts_cierre)
#Marca que la serie de tiempo tiene menos de dos periodos: no se apreciarian los componentes
#Se necesita que la serie muestre mas de dos componentes para que pueda hacer una reconstruccion de la misma en cada uno 
#de los componentes 
#La forma correcta deberia ser así:
#seasonal_preciocierre<-ts_cierre_componentes$seasonal # valores estimados de estacionalidad 
#trend_preciocierre<-ts_cierre_componentes$$trend # valores estimados de tendencia 
#random_preciocierre<-ts_cierre_componentes$random #valores estimados de aleatoriedad

#NOTA: CORRER EL CODIGO ANTERIOR CON LA SERIE DIARIA y DESPUES EJECUTAR ESTA FUNCIÓN 
#PRECIOCIERRE_est<-seasonal_preciocierre+trend_preciocierre+random_preciocierre
#la serie diaria deberia permitir la separación de la serie en componentes