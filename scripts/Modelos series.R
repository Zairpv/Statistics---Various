#-------------------------------------------------------------------
###1. Consola
#-------------------------------------------------------------------
rm(list=ls()) 
options(max.print = 1000000000) 
setwd("D:/zaira/Documents/08. R scripts/Series - Precios cafe")

library(tseries)


datos_anual<-read.csv("Cafe_anual.csv",header=TRUE) 
View(datos_anual)
datos_mensual<-read.csv("Cafe_mensual.csv",header=TRUE) 
View(datos_mensual)
datos_diario<-read.csv("cafe_diario_v5.csv",header=TRUE) 
View(datos_diario)



#-------------------------------------------------------------------
###2. Series de tiempo
#-------------------------------------------------------------------
##Anual
Anual_cierre <- ts(datos_anual$cierre,start = 1980,end = 2022,frequency = 1)
Anual_cierre

##Mensual
Mensual_cierre<-ts(datos_mensual$cierre,start = 1980,end = 2023,frequency = 12)
Mensual_cierre

##Diario
#Antes de definir la serie debemos hacer como una "plantilla de calendario" donde se defina la sequencia de dias
#Al final si agregue los días 29 de febrero a la base, con los datos de la serie original q me diste. 
inds <- seq(as.Date("1980-01-01"), as.Date("2022-12-31"), by = "day")

Diario_cierre<-ts(datos_diario$cierre,start = c(1980, as.numeric(format(inds[1], "%j"))),
                  frequency = 365)
Diario_cierre #Ahora si la serie contiene todos los datos



#-------------------------------------------------------------------
### 3. Analisis descriptivo
#-------------------------------------------------------------------
summary(Anual_cierre)
summary(Mensual_cierre)
summary(Diario_cierre)
#Nota. Si observas, el valor medio en las tres series es muy muy similar, con 127. 
#Verifiquemos que los valores NA no alteren las estadisticas
summary(Diario_cierre,na.action(na.omit())) #Aqui hasta nos da recuento de los NA

var(Anual_cierre,na.rm = T)
var(Mensual_cierre,na.rm = T)
var(Diario_cierre,na.rm = T)

sd(Anual_cierre,na.rm = T)
sd(Mensual_cierre,na.rm = T)
sd(Diario_cierre,na.rm = T)

library(moments)
kurtosis(Anual_cierre,na.rm = T)
kurtosis(Mensual_cierre,na.rm = T)
kurtosis(Diario_cierre,na.rm = T)

skewness(Anual_cierre,na.rm = T)
skewness(Mensual_cierre,na.rm = T)
skewness(Diario_cierre,na.rm = T)

#Histogramas
par(mfrow=c(1,3),cex=0.9) 
hist(Anual_cierre,main="a)",ylab="Frecuencia",xlab="Precio de cierre",sub="Serie anual") 
hist(Mensual_cierre,main = "b)",ylab = "Frecuencia",xlab="Precio de cierre",sub="Serie mensual")
hist(Diario_cierre,main = "c)",ylab = "Frecuencia",xlab="Precio de cierre",sub="Serie diaria")
dev.off()

#Densidad
par(mfrow=c(1,3),cex=0.9) 
plot(density(Anual_cierre,na.rm = TRUE), main = "a)",ylab="Densidad",sub="Serie anual")
plot(density(Mensual_cierre,na.rm = TRUE), main = "b)",ylab="Densidad",sub="Serie mensual")
plot(density(Diario_cierre,na.rm = TRUE), main = "c)",ylab="Densidad",sub="Serie diaria")
dev.off()

#Gráficas
plot.ts(Anual_cierre,ylab="Precio de cierre",xlab="Año",sub="Serie anual")
plot.ts(Mensual_cierre,ylab="Precio de cierre",xlab="Año",sub="Serie mensual")
plot.ts(Diario_cierre,ylab="Precio de cierre",xlab="Año",sub="Serie diaria")

library(ggplot2)
autoplot(Anual_cierre,xlab = "Año",ylab = "Precio de cierre",main = "Serie anual")
autoplot(Mensual_cierre,xlab = "Año",ylab = "Precio de cierre",main = "Serie mensual")
autoplot(Diario_cierre,xlab = "Año",ylab = "Precio de cierre",main = "Serie diaria")

#Nota: Como puedes observar, todas las series son similares en los patrones temporales. Por lo que solo podrias trabajar con una de ellas. Aparentemente la mejor es la mensual xk la diaria tiene variaciones por los NA 
#Nota. Las series muestran estacionalidad y variaciones importantes a traves del tiempo por lo que podrian no ser estacionarias


#-------------------------------------------------------------------
### 4. Función de autocorrelación y pacf
#-------------------------------------------------------------------
#Nota: En una serie estacionaria se espera un ACF que va a cero para cada lag o cambio en el tiempo. 
#Nota. Si hay lags que estan por arriba de la linea punteada (intervalos de confianza del ACF) -> Serie no estacionaria

acf(Anual_cierre,na.action = na.pass) #Podria ser estacionaria diferenciandola una vez
acf(Mensual_cierre,na.action = na.pass) #Serie no estacionaria
acf(Diario_cierre,na.action = na.pass) #Serie no estacionaria

#Otra forma de gráficar
autoplot(acf(Anual_cierre,na.action = na.pass))
autoplot(acf(Mensual_cierre,na.action = na.pass))
autoplot(acf(Diario_cierre,na.action = na.pass))


pacf(Anual_cierre,na.action = na.pass)
pacf(Mensual_cierre,na.action = na.pass)
pacf(Diario_cierre_sinNA) #La defini más adelante, en la linea 133

#-------------------------------------------------------------------
### 5. Descomposición de las series de tiempo
#-------------------------------------------------------------------
#Nota. Siempre es recomendable analizar los componentes de la serie de tiempo

### Descomposición aditiva de una serie:

#Serie anual
autoplot(Anual_cierre,xlab = "Año",ylab = "Precio de cierre",main = "Serie anual")
decompose(Anual_cierre) #No tiene más de dos periodos - por lo tanto no puede obtenerse sus componentes
#En estas series no se puede tampoco hacer un suavizamiento por promedios móviles para mejorar la observación de las tendencias


#Serie mensual
mensual_componentes=decompose(Mensual_cierre)
autoplot(mensual_componentes)
#La grafica de tendencia muestra un ligero incremento del precio a traves del periodo y una marcada estacionalidad cada 10 años aprox
#El componente aleatorio tiene mucho ruido en esos peridos de incrementos.... posiblemente un evento importante explique ese ruido (talvez elecciones o alza en petroleo... ¿?)

#Serie diaria

diaria_componentes=decompose(Diario_cierre_sinNA)
autoplot(diaria_componentes)
#Se observa claramente que los componentes son iguales entre la serie mensual y diaria. Con excepción es que como la serie diaria al tener mas datos es mas densa la gráfica
#Conclusión: puedes trabajar con solo una de las series. Te recomiendo la serie mensual



#-------------------------------------------------------------------
### 6. Reconstrucción de una serie en función de sus componentes
#-------------------------------------------------------------------

#Mensual
x_mensual=mensual_componentes$x #Datos originales
seasonal_mensual=mensual_componentes$seasonal #valores estimados de estacionalidad
trend_mensual=mensual_componentes$trend #Valores estimados de tendencia
random_mensual=mensual_componentes$random #Valores estimados de aleatoriedad

mensual_est=seasonal_mensual+trend_mensual+random_mensual
mensual_est=ts(mensual_est,frequency = 12,start = 1980,end=2023,names = "Precio de cierre Est")
autoplot(mensual_est,xlab = "Año",ylab = "Precio de cierre")

#Gráfica de x: serie original vs y: estimación con modelo aditivo
plot.ts(Mensual_cierre,mensual_est,xlab="Año",ylab="Precio de cierre")
both_mensual=cbind(Mensual_cierre,mensual_est) #Con esto se juntan ambas series
plot.ts(both_mensual,plot.type = c("multiple","single"),col="darkgreen")


#Diaria
x_diaria=diaria_componentes$x #Datos originales
seasonal_diaria=diaria_componentes$seasonal #valores estimados de estacionalidad
trend_diaria=diaria_componentes$trend #Valores estimados de tendencia
random_diaria=diaria_componentes$random #Valores estimados de aleatoriedad

diaria_est=seasonal_diaria+trend_diaria+random_diaria
diaria_est=ts(diaria_est,start = c(1980, as.numeric(format(inds[1], "%j"))),
              frequency = 365,names = "Precio de cierre Est")
autoplot(diaria_est,xlab = "Año",ylab = "Precio de cierre") 
#NOTA IMPORTANTE: Este es uno de los problemas que tiene trabajar con la serie diaria. 
#Debido a la remoción de valores de NA, la definición de otros objetos que esten basados en la serie sin NA provocará que salga mal el periodo
#En la gráfica te pone que va de 1980 a 2010 cuando debe ir hasta 2022
#Al continuar con el resto del código pues se remarca mucho ese error:
#Gráfica de x: serie original vs y: estimación con modelo aditivo
plot.ts(Diario_cierre,diaria_est,xlab="Año",ylab="Precio de cierre")
both_diario=cbind(Diario_cierre,diaria_est) #Con esto se juntan ambas series
plot.ts(both_diario,plot.type = c("multiple","single"),col="darkgreen")
#Conclusión: debido a la presencia de NA no se puede reconstruir la serie diaria a partir de sus componentes



#-------------------------------------------------------------------
### 7. Remoción de sus componentes
#-------------------------------------------------------------------

#Mensual
mensual_adjust=Mensual_cierre-seasonal_mensual #Quita la estacionalidad
plot(mensual_adjust)
mensual_adjust=Mensual_cierre-trend_mensual #Quita la tendencia
plot(mensual_adjust)
mensual_adjust=Mensual_cierre-(trend_mensual+random_mensual) #quita la tendencia y aleatoriedad
plot(mensual_adjust) 

#Diaria
diaria_adjust=Diario_cierre_sinNA-seasonal_diaria #Quita la estacionalidad
plot(diaria_adjust)
diaria_adjust=Diario_cierre_sinNA-trend_diaria #Quita la tendencia
plot(diaria_adjust)
diaria_adjust=Diario_cierre_sinNA-(trend_diaria+random_diaria) #quita la tendencia y aleatoriedad
plot(diaria_adjust) 



#-------------------------------------------------------------------
### 8. Descomposición con modelo multiplicativo
#-------------------------------------------------------------------
# Y[t] = T[t] * S[t] * e[t]

#Mensual
mensual_mult_components=decompose(Mensual_cierre,type = "multiplicative")
x_mensual_mult=mensual_mult_components$x
seasonal_mensual_mult=mensual_mult_components$seasonal
trend_mensual_mult=mensual_mult_components$trend
random_mensual_mult=mensual_mult_components$random

mensual_seasonal2=x_mensual_mult/seasonal_mensual_mult
plot(mensual_seasonal2)
mensual_trend2=x_mensual_mult/trend_mensual_mult
plot(mensual_trend2)
mensual_random2=x_mensual_mult/(seasonal_mensual_mult*trend_mensual_mult)
plot(mensual_trend2)

#Diaria
diaria_mult_components=decompose(Diario_cierre_sinNA,type = "multiplicative")
x_diaria_mult=diaria_mult_components$x
seasonal_diaria_mult=diaria_mult_components$seasonal
trend_diaria_mult=diaria_mult_components$trend
random_diaria_mult=diaria_mult_components$random

diaria_seasonal2=x_diaria_mult/seasonal_diaria_mult
plot(diaria_seasonal2)
diaria_trend2=x_diaria_mult/trend_diaria_mult
plot(diaria_trend2)
diaria_random2=x_diaria_mult/(seasonal_diaria_mult*trend_diaria_mult)
plot(diaria_trend2)



#-------------------------------------------------------------------
### 9. Comparación entre años
#-------------------------------------------------------------------
#Se hace para identificar variaciones mensuales entre los diferentes años evaluados 
#e identificar visualmente posibles tendencias por periodicidad
library(TSstudio)

#Types:
#Normal: comparación entre años
#Cycle: identificación de tendencias
#Box: boxplots
#All: todos juntos

#Anual
ts_seasonal(Anual_cierre,type = "normal") #No se puede por la misma razon anterior, no puede extraerse componentes de esta serie

#Mensual
ts_seasonal(Mensual_cierre,type = "normal")
ts_seasonal(Mensual_cierre,type = "cycle")
ts_seasonal(Mensual_cierre,type = "box")
ts_seasonal(Mensual_cierre,type = "all")

#Diaria
ts_seasonal(Diario_cierre,type = "normal")
ts_seasonal(Diario_cierre,type = "cycle")
ts_seasonal(Diario_cierre,type = "box")
ts_seasonal(Diario_cierre,type = "all")
#Otro problema de los NA :/



#-------------------------------------------------------------------
### 10. Cambios estructurales de una serie de tiempo
#-------------------------------------------------------------------
#los breaks o puntos de corte minimizan la suma de cuadrados residuales asociadas a un modelo 
#con m+1 segmentos, dado un tamaño minimo de segmento de h*n observaciones. Por lo cual es necesario
#calcular el numero de breaks optimos, el ancho de banda y elegir el modelo que minimiza el criterio BIC

library(strucchange)
#ancho de banda: ventaja que toma el porcentaje de los punto spara calcular los segmentos de ajuste de los modelos,
#                generalmente es alrededor del 10 al 15% (h=0.1). 


## a) Identificación de cambio de nivel con modelo lineal

#Anual
summary(lm(Anual_cierre~1)) #Es significativo el intercepto
plot(Anual_cierre,col="dark gray",main="serie anual")
lines(ts(fitted(lm(Anual_cierre~1)),start = 1980,frequency = 1),col=2)
grid()
anual_breaks_level=breakpoints(Anual_cierre~1,h=0.1)
summary(anual_breaks_level) 
#El menor BIC es cuando m = 2
plot(anual_breaks_level,sub="Serie anual") #breaks = 2
#Series, cambios de nivel e intervalos de confianza de los cambios de nivel: 
plot(Anual_cierre,col="light gray",main="Serie anual")
lines(fitted(anual_breaks_level,breaks = 2),col="red")
lines(confint(anual_breaks_level,breaks = 2))
coef(anual_breaks_level,breaks = 2)


#Mensual
summary(lm(Mensual_cierre~1)) #Es significativo el intercepto
plot(Mensual_cierre,col="dark gray",main="Serie mensual")
lines(ts(fitted(lm(Mensual_cierre~1)),start = 1980,frequency = 1),col=2)
grid()
mensual_breaks_level=breakpoints(Mensual_cierre~1,h=0.1)
summary(mensual_breaks_level) 
#El menor BIC es cuando m = 6
plot(mensual_breaks_level,sub="Serie mensual") #breaks = 6
#Series, cambios de nivel e intervalos de confianza de los cambios de nivel: 
plot(Mensual_cierre,col="light gray",main="Serie mensual")
lines(fitted(mensual_breaks_level,breaks = 6),col="red")
lines(confint(mensual_breaks_level,breaks = 6))
coef(mensual_breaks_level,breaks = 6)


#Diaria
summary(lm(Diario_cierre~1,na.action = na.omit)) #Es significativo el intercepto
plot(Diario_cierre,col="dark gray",main="Serie diaria")
lines(ts(fitted(lm(Diario_cierre~1,na.action = na.omit)),start = c(1980, as.numeric(format(inds[1], "%j"))),
         frequency = 365),col=2) #NOTA. NUEVAMENTE TENEMOS PROBLEMAS PARA LA DEFINICIO DEL NIVEL CON LA SERIE
grid()
#Veamos si con la serie sin NA esto se soluciona
summary(lm(Diario_cierre_sinNA~1)) #Es significativo el intercepto
plot(Diario_cierre_sinNA,col="dark gray",main="Serie diaria")
lines(ts(fitted(lm(Diario_cierre_sinNA~1)),start = c(1980, as.numeric(format(inds[1], "%j"))),
         frequency = 365),col=2) #NOTA. Tampoco se soluciona. 
grid()

diaria_breaks_level=breakpoints(Diario_cierre_sinNA~1,h=0.1) #Tarda un rato en procesar por la cantidad de datos
summary(diaria_breaks_level) 
#El menor BIC es cuando m = ?
plot(diaria_breaks_level,sub="Serie diaria") #breaks = ?
#Series, cambios de nivel e intervalos de confianza de los cambios de nivel: 
plot(Diario_cierre_sinNA,col="light gray",main="Serie diaria")
lines(fitted(diaria_breaks_level,breaks = ),col="red")
lines(confint(mensual_breaks_level,breaks = ))
coef(mensual_breaks_level,breaks = )

#Me tardo como dos horas en cargar, asi que una propuesta aqui es utilizar una ventana de la serie que represente un periodo más corto
diaria_2000_2022=window(Diario_cierre, start = 2000) #NOTA. Esta es la forma correcta para trabajar un periodo más corto de tu serie
diaria_breaks_level_window=breakpoints(diaria_2000_2022~1,h=0.1) 
summary(diaria_breaks_level_window) 
#El menor BIC es cuando m = ?
plot(diaria_breaks_level_window,sub="Serie diaria (2000 - 2022) ") #breaks = ?
#Series, cambios de nivel e intervalos de confianza de los cambios de nivel: 
plot(Diario_cierre_sinNA,col="light gray",main="Serie diaria")
lines(fitted(diaria_breaks_level_window,breaks = ),col="red")
lines(confint(diaria_breaks_level_window,breaks = ))
coef(diaria_breaks_level_window,breaks = )



## b) Identificación de cambio de tendencia con modelo lineal

#Para identificar cambios de tendencia nuevamente se ajusto el modelo,
#incorporando el tiempo en el ajuste lineal en lugar del intercepto.

#Anual
#Ajuste de modelo de regresión lineal: 
l_anual=length(Anual_cierre)
tt_anual=1:l_anual
trend_fit_anual=lm(Anual_cierre~tt_anual)
summary(trend_fit_anual) #El intercepto es significativo pero el tiempo no
plot(Anual_cierre,col="light gray",main="Serie anual")
lines(ts(fitted(trend_fit_anual),start = 1980,frequency = 1),col=2)
#Breaks optimos: 
anual_breaks_trend=breakpoints(Anual_cierre~tt_anual,h=0.1)
summary(anual_breaks_trend) #5 breaks 
plot(anual_breaks_trend) #5 breaks
breakdates(anual_breaks_trend,breaks = 5)
coef(anual_breaks_trend,breaks = 5)
plot(Anual_cierre,col="light gray",main="Serie anual")
lines(fitted(anual_breaks_trend,breaks = 5),col=2)
lines(confint(anual_breaks_trend,breaks = 5))
#El ultimo intervalo de 2018 no me convence, posiblemente sea por lo cual no es significativa el tiempo en el modelo


#Mensual
#Ajuste de modelo de regresión lineal: 
l_mensual=length(Mensual_cierre)
tt_mensual=1:l_mensual
trend_fit_mensual=lm(Mensual_cierre~tt_mensual)
summary(trend_fit_mensual) #Ambas son significativas
plot(Mensual_cierre,col="light gray",main="Serie mensual")
lines(ts(fitted(trend_fit_mensual),start = 1980,frequency = 12),col=2)
#Breaks optimos: 
mensual_breaks_trend=breakpoints(Mensual_cierre~tt_mensual,h=0.1)
summary(mensual_breaks_trend) #8 breaks 
plot(mensual_breaks_trend) #8 breaks
breakdates(mensual_breaks_trend,breaks = 8)
coef(mensual_breaks_trend,breaks = 8)
plot(Mensual_cierre,col="light gray",main="Serie mensual")
lines(fitted(mensual_breaks_trend,breaks = 8),col=2)
lines(confint(mensual_breaks_trend,breaks = 8))
#Esta grafica si esta mejor definida que la anual


#Diaria
#Ajuste de modelo de regresión lineal: 
l_diaria=length(Diario_cierre_sinNA)
tt_diaria=1:l_diaria
trend_fit_diaria=lm(Diario_cierre_sinNA~tt_diaria)
summary(trend_fit_diaria) #Ambas son significativas
plot(Diario_cierre_sinNA,col="light gray",main="Serie diaria")
lines(ts(fitted(trend_fit_diaria),start = c(1980, as.numeric(format(inds[1], "%j"))),
         frequency = 365),col=2) #Seguimos teniendo ese problema por los NA, no nos estima el intervalo completo

#Breaks optimos: 
diaria_breaks_trend=breakpoints(Diario_cierre_sinNA~tt_diaria,h=0.1)
summary(diaria_breaks_trend) # ? breaks
plot(diaria_breaks_trend) #? breaks
breakdates(diaria_breaks_trend,breaks = )
coef(diaria_breaks_trend,breaks = )
plot(Diario_cierre_sinNA,col="light gray",main="Serie mensual")
lines(fitted(diaria_breaks_trend,breaks = ),col=2)
lines(confint(diaria_breaks_trend,breaks = ))


#-------------------------------------------------------------------
### 11. ARIMA
#-------------------------------------------------------------------
#Un modelo arima no estacional pdq 
#p=terminos autorregresivos
#d=numero de diferencias no estacionales necesarias para la estacionariedad
#q= numero de errores de pronostico rezagados en la ecuacion de prediccion

library(stats)
library(ggfortify)
library(ggpubr)

#Modelos autorregresivos (AR(1))
auto.arima(Anual_cierre,d=1,stationary = T,seasonal = T)
(M1_ANUAL=arima(Anual_cierre,order = c(1,0,0))) #Con intercepto
ggtsdiag(M1_ANUAL) #Esta es para diagnostico del modelo
#No debehaber rezagos en la grafica para considerarlos ruido blanco - EN ACF HAY 1. 

(M2_ANUAL=arima(Anual_cierre,order = c(1,0,0),include.mean = FALSE)) #sin intercepto
ggtsdiag(M2_ANUAL)
#Modelos media movil 
(M3_ANUAL=arima(Anual_cierre,order = c(0,0,1))) #Con intercepto
ggtsdiag(M3_ANUAL)
(M4_ANUAL=arima(Anual_cierre,order = c(0,0,1),include.mean = FALSE)) #sin intercepto
ggtsdiag(M4_ANUAL) #Este modelo esta mal, hay mucho rezago
#Modelos ARMA (1,1)
(M5_ANUAL=arima(Anual_cierre,order = c(1,0,1))) #Con intercepto
ggtsdiag(M5_ANUAL)
(M6_ANUAL=arima(Anual_cierre,order = c(1,0,1),include.mean = FALSE)) #sin intercepto
ggtsdiag(M6_ANUAL)
#Con una diferenciacion
(M7_ANUAL=arima(Anual_cierre,order = c(1,1,1))) #Con intercepto
ggtsdiag(M7_ANUAL)
(M8_ANUAL=arima(Anual_cierre,order = c(1,1,1),include.mean = FALSE)) #sin intercepto
ggtsdiag(M8_ANUAL)

BIC(M1_ANUAL,M2_ANUAL,M3_ANUAL,M4_ANUAL,M5_ANUAL,M6_ANUAL,M7_ANUAL,M8_ANUAL)
#Escoger menor BIC

ggtsdiag(M1_ANUAL)


#Recordemos que este analisis falta checar supuestos de normalidad 
