###1. Consola
rm(list=ls()) 
options(max.print = 100000000) 
setwd("D:/zaira/Documents/08. R scripts/Series - Precios cafe")
library(tseries)
library(ggfortify)
library(fBasics)
library(car)
library(tidyverse)
library(ggplot2)
library(Rmisc)
library(forecast)
library(nortest)
library(strucchange)
library(tsoutliers)
library(fUnitRoots)
library(astsa)
library(dplyr)
library(lmtest)
library(aTSA)
library(TSA)



###2. Datos
datos_anual<-read.csv("Cafe_anual.csv",header=TRUE) 
View(datos_anual)
datos_mensual<-read.csv("Cafe_mensual.csv",header=TRUE) 
View(datos_mensual)
datos_diario<-read.csv("Cafe_diario.csv",header=TRUE) 
View(datos_diario)



###3. Creación de la serie
#Anual
names(datos_anual)
Anual_cierre <- ts(datos_anual$cierre,start = 1980,end = 2022,frequency = 1)
Anual_cierre
Anual_apertura<-ts(datos_anual$apertura,start = 1980,end = 2022,frequency = 1)
Anual_apertura
#Mensual
names(datos_mensual)
Mensual_cierre <- ts(datos_mensual$cierre,start = 1980,end = 2023,frequency = 12)
Mensual_cierre
Mensual_apertura <- ts(datos_mensual$apertura,start = 1980,end = 2023,frequency = 12)
Mensual_apertura
#Diaria
names(datos_diario)
#Correr lo siguiente: una vez que esten definidos los NA de sabados y domingos---- 
#Para años bisiestos creo q es recomendable quitar el dia 29 de febrero
#Te recomiendo mejor quitar los datos 2023 para que todas las series terminen en diciembre 2022, en la diaria sería el 31 de dic
#Veo que la serie diaria empieza el 2 de enero de 1980, entonces hay que poner un valor NA antes para el dia 1 de enero, asi se completa el periodo completo en todas las series
#Esta la forma correcta para definir la serie con los datos ajustados en el excel: 
#Diario_cierre <- ts(datos_diario$cierre,start = c(1980,1,1),end = c(2022,12,31),frequency = 365) 
#Diario_cierre 
#print(Diario_cierre,calendar = T)
#plot(Diario_cierre) 
#Diario_apertura <- ts(datos_diario$apertura,start = c(1980,1,1),end = c(2022,12,31),frequency = 365) 
#Diario_apertura 
#print(Diario_apertura,calendar = T)
#plot(Diario_apertura) 



###4. Analisis exploratorio
basicStats(Anual_cierre) #Esta función t da todos los descriptivos de las series 
basicStats((Anual_apertura))
basicStats(Mensual_cierre)
basicStats(Mensual_apertura)
#basicStats(Diario_cierre)
#basicStats(Diario_apertura)

#Verificar como van los patrones entre ambos tipos de precios
plot(Anual_cierre,xlab = "Año",ylab="Precio de cierre ($)")
grid()
plot(Anual_apertura,xlab = "Año",ylab="Precio de apertura ($)")
grid()
#Se mantienen, entonces podria solo abordarse una sola variable

plot(Mensual_cierre,xlab = "Año",ylab="Precio de cierre ($)")
grid()
plot(Mensual_apertura,xlab = "Año",ylab="Precio de apertura ($)")
grid()
#Lo mismo que la serie anual, los patrones se mantienen

#A partir de aqui haré la evaluación solo para precio de cierre
par(mfrow=c(2,2),mar=c(5,4,3,1),cex=0.6)
plot(Anual_cierre,xlab = "Año",ylab="Precio de cierre ($)",main="a") #Serie de tiempo
grid()
hist(Anual_cierre,main = "b)",xlab ="Precio de cierre",ylab = "Frecuencia") #Histograma de frecuencias
grid()
plot(density(Anual_cierre), main = "c)",ylab = "Densidad") #Distribución de probabilidad
grid()
qqPlot(Anual_cierre,dist="norm",ylab = "Precio de apertura ($)",main = "d)") #Ajuste de normalidad
grid()
dev.off()
jarque.bera.test(Anual_cierre)
#H0: Los datos se distribuyen normal -> con un pvalue > 0.05 no se rechaza H0
#Los datos son normales

#Mensual
par(mfrow=c(2,2),mar=c(5,4,3,1),cex=0.6)
plot(Mensual_cierre,xlab = "Año",ylab="Precio de cierre ($)",main="a") #Serie de tiempo
grid()
hist(Mensual_cierre,main = "b)",xlab ="Precio de cierre",ylab = "Frecuencia") #Histograma de frecuencias
grid()
plot(density(Mensual_cierre), main = "c)",ylab = "Densidad") #Distribución de probabilidad
qqPlot(Mensual_cierre,dist="norm",ylab = "Precio de apertura ($)",main = "d)") #Ajuste de normalidad
grid()
dev.off()
jarque.bera.test(Mensual_cierre)
#H0: Los datos se distribuyen normal -> con un pvalue menor a 0.05 si se rechaza H0
#Los datos no son normales

#Si comparamos el gráfico a) de las series mensual y anual vemos que el patron realmente es el mismo, obviamente con mejor detalle a nivel mensual
#eso quiere decir que podrias unicamente evaluar la serie a escala mensual ya que esta si puede descomponerse y la anual no



###5. Transformación de las series
par(mfrow=c(2,2),mar=c(5,4,3,1),cex=0.6)
jarque.bera.test((Mensual_cierre)^2) #Transformación cuadrática ->#No cumple normalidad
qqPlot((Mensual_cierre)^2,dist="norm",ylab = "Precio de cierre ($)",main = "transformación cuadrática") #Ajuste de normalidad

jarque.bera.test(exp(Mensual_cierre)) #La serie No cumple las condiciones para una transformación exponencial
qqPlot(exp(Mensual_cierre),dist="norm",ylab = "Precio de cierre ($)",main = "Transformación exponencial") #Ajuste de normalidad

jarque.bera.test(log(Mensual_cierre)) #Transformación logaritmica: aun no alcanza normalidad
qqPlot(log(Mensual_cierre),dist="norm",ylab = "Precio de cierre ($)",main = "Transformación logarítmica") #Ajuste de normalidad

jarque.bera.test(sqrt(Mensual_cierre)) #Transformación raiz cuadrada: si alcanza normalidad
qqPlot(sqrt(Mensual_cierre),dist="norm",ylab = "Precio de cierre ($)",main = "Transformación raíz cuadrada") #Aunque la gráfica no es del todo convincente
dev.off()

#Y si probamos una transf BoxCox (coN la libreria forecast)
?BoxCox.lambda
(lambda_CierreMensual_1=BoxCox.lambda(Mensual_cierre,method = "guerrero"))
(CierreMensual_BC_1=BoxCox(Mensual_cierre,lambda = lambda_CierreMensual_1))
jarque.bera.test(CierreMensual_BC_1) #No se alcanza la normalidad con el método guerrero
qqPlot(CierreMensual_BC_1,dist="norm",ylab = "Precio de cierre ($)",main = "Transformación BoxCox1")

#Probando con el método "loglik" 
(lambda_CierreMensual_2=BoxCox.lambda(Mensual_cierre,method = "loglik"))
#Lambda = 0.3
(CierreMensual_BC_2=BoxCox(Mensual_cierre,lambda = lambda_CierreMensual_2))
jarque.bera.test(CierreMensual_BC_2) #Con el método "Loglik"  si se alcanza la normalidad
qqPlot(CierreMensual_BC_2,dist="norm",ylab = "Precio de cierre ($)",main = "Transformación BoxCox2") 

#Nota: con la transformación sqrt y boxcox se alcanza normalidad
#Comparando la prueba jarque bera, es mejor quedarse con la transformación BOXCOX ya que el pvalue es más significativo
jarque.bera.test(sqrt(Mensual_cierre)) #Pvalue 0.087
jarque.bera.test(CierreMensual_BC_2) #Pvalue 0.991
par(mfrow=c(1,2),mar=c(5,4,3,1),cex=0.6)
qqPlot(sqrt(Mensual_cierre),dist="norm",ylab = "Precio de cierre ($)",main = "Transformación raíz cuadrada") #Aunque la gráfica no es del todo convincente
qqPlot(CierreMensual_BC_2,dist="norm",ylab = "Precio de cierre ($)",main = "Transformación BoxCox2") #Aunque la gráfica no es del todo convincente
dev.off()

#Comparaci?n entre transformaciones
par(mfrow=c(2,3),mar=c(5,4,3,1),cex=0.6)
hist(Mensual_cierre,main = expression("Original"),xlab = "Precio de cierre ($)",ylab = "Frecuencia")
hist(sqrt(Mensual_cierre),main = expression("Raíz cuadrada"),xlab = "Precio de cierre ($)",ylab = "Frecuencia")
hist(CierreMensual_BC_2,main = expression(paste("BoxCox (", lambda, " = 0.3)")),xlab = "Precio de Cierre ($)",ylab = "Frecuencia")

qqPlot(Mensual_cierre,dist="norm",ylab = "Precio de Cierre ($)",main = "")
qqPlot(sqrt(Mensual_cierre),dist="norm",ylab = "Precio de Cierre ($)",main = "")
qqPlot(CierreMensual_BC_2,dist="norm",ylab = "Precio de Cierre ($)",main = "")
dev.off()



###6. Identificación de la señal y nivel de la serie 

#Serie anual
#a) señal de la serie
par(mfrow=c(2,1),mar=c(4.5,4.6,1.7,1),cex=0.6)
tt_Cierre_Anual<-1:length(Anual_cierre)
fit_Cierre_Anual<-ts(loess(Anual_cierre~tt_Cierre_Anual,span = 0.2)$fitted,start = 1980,frequency=1)
plot.ts(fit_Cierre_Anual,type="l",main="a)",ylab="Precio de cierre ($)")
grid()
lines(Anual_cierre,col="Red")
#b) Nivel de la serie
fitlevel_Cierre_Anual<-lm(Anual_cierre~1)
summary(fitlevel_Cierre_Anual)
plot.ts(Anual_cierre,main="b)",ylab="Precio de cierre ($)")
lines(ts(fitted(fitlevel_Cierre_Anual),start = 1980,frequency = 1),col="red")
dev.off()

#Señal mensual (considerando transformación BoxCox)
## a) Señal de la serie
par(mfrow=c(2,1),mar=c(4.5,4.6,1.7,1),cex=0.6)
tt_Cierre_Mensual<-1:length(CierreMensual_BC_2)
fit_Cierre_Mensual<-ts(loess(CierreMensual_BC_2~tt_Cierre_Mensual,span = 0.1)$fitted,start = 1980,frequency=12)
plot.ts(fit_Cierre_Mensual,type="l",main="a)",ylab=expression(paste("Precio de cierre (", lambda, " = 0.3)")))
grid()
lines(CierreMensual_BC_2,col="Red")
## b) Nivel de la serie
fitlevel_Cierre_Mensual<-lm(CierreMensual_BC_2~1)
summary(fitlevel_Cierre_Mensual)
plot.ts(CierreMensual_BC_2,main="b)",ylab=expression(paste("Precio de cierre", lambda, " = -0.3)")))
lines(ts(fitted(fitlevel_Cierre_Mensual),start = 1980,frequency = 12),col="red")
grid()
dev.off()



###7. Raíz unitaria de las series
#Anual
par(mfrow=c(2,1),mar=c(5.1,4.9,1.7,1),cex=0.6)
plot(Anual_cierre,sub="Serie original",ylab = "Precio de cierre",xlab="Año")
plot(diff(Anual_cierre),sub="Serie diferenciada (d=1)",ylab = "Precio de cierre",xlab="Año")
acf(Anual_cierre,main="a)")
acf(diff(Anual_cierre),main="b)",sub="d=1")
mod1<-ar(diff(Anual_cierre),method = "mle")
mod1$order #Fue = 0
#Hay constante? es significativa?
t_CierreAnual=seq(2:length(Anual_cierre))
mod2<-lm(diff(Anual_cierre)~t_CierreAnual)
summary(mod2) #No es significativa
#Ajustar a 0 lags
adfTest(Anual_cierre,lags = 0,type = "nc")
adfTest(diff(Anual_cierre),lags = 0,type = "nc")
#La serie transformada no tiene raiz unitaria y por lo tanto no necesita diferenciarse
ndiffs(Anual_cierre)
dev.off()

#Mensual
par(mfrow=c(2,1),mar=c(5.1,4.9,1.7,1),cex=0.6)
plot(CierreMensual_BC_2,sub="Serie original con transformación Boxcox",xlab="Año",ylab = expression(paste("Precio de cierre (", lambda, " = 0.3)")))
plot(diff(CierreMensual_BC_2),sub="Serie transformada y diferenciada (d=1)",ylab = expression(paste("Precio de cierre (", lambda, " = -0.4164)")),xlab="Año")
acf(CierreMensual_BC_2,main="a)")
acf(diff(CierreMensual_BC_2),main="b)",sub="d=1")
mod1<-ar(diff(CierreMensual_BC_2),method = "mle")
mod1$order #Fue = 2
#Hay constante? es significativa?
t_CierreMensual=seq(2:length(CierreMensual_BC_2))
mod2<-lm(diff(CierreMensual_BC_2)~t_CierreMensual)
summary(mod2) #No es significativa
#Ajustar a 2 lags
adfTest(CierreMensual_BC_2,lags = 2,type = "nc")
adfTest(diff(CierreMensual_BC_2),lags = 2,type = "nc")
#La serie transformada tiene raiz unitaria y por lo tanto necesita diferenciarse
ndiffs(CierreMensual_BC_2) #Basta 1 diferenciación
dev.off()



###8. ACF Y PACF
acf(diff(CierreMensual_BC_1))
pacf(diff(CierreMensual_BC_2))
acf2(diff(CierreMensual_BC_2),main = "Serie: Precios de cierre mensuales (d=1)")

## a) Modelos arima 
Arima1_model<- arima(CierreMensual_BC_2, order=c(1,1,0),seasonal = list(order = c(0,1,1), period = 12),method="ML")
coeftest(Arima1_model)

Arima2_model<- arima(CierreMensual_BC_2, order=c(1,0,0),seasonal = list(order = c(0,1,1), period = 12),method="ML")
coeftest(Arima2_model) #Mejor modelo

Arima3_model<- arima(CierreMensual_BC_2, order=c(1,1,0),seasonal = list(order = c(1,1,0), period = 12),method="ML")
coeftest(Arima3_model)

## b) Diagnostico del modelo
ggtsdiag(Arima2_model)
Box.test(Arima2_model$residuals) # Test de Box-Pierce #Independencia
Box.test(Arima2_model$residuals, type="Ljung-Box") #Independencia
jarque.bera.test(Arima2_model$residuals) #Normalidad No
par(mfrow=c(1,2),cex=0.9,mar=c(5,4,3,1))
hist(Arima2_model$residuals, main="a)",xlab = "Residuales",ylab = "Frecuencia")
qqPlot(Arima2_model$residuals,distribution = "norm",main = "b)",ylab = "Residuales")
dev.off()


## c) Identificaci?n de outliers en serie
tso(CierreMensual_BC_2,types = c("AO","LS","TC"))
plot(tso(CierreMensual_BC_2,types = c("AO","LS","TC")))
#Se identifico un outlier LS (cambio de nivel)

## d) y en residuales?
res_Arima1_x<-residuals(Arima2_model)
polin_X1<-coefs2poly(Arima2_model)
(outliersX1<-locate.outliers(res_Arima1_x,polin_X1)) 
#Nota. esta funci?n solo se utiliza con residuales
plot(tso(res_Arima1_x))


#Aqui te recomiendo evaluar si existio algun evento estocastico en esos puntos donde se identifican los outliers
#En este caso podrias definir una variable de intervención binaria
#Te anexo mi trabajo final con los codigos para q cheques como aborde esos problemas

#Este modelo arima veo que se ajusta muy muy bien xk esta serie solo necesito una diferenciacion y los valores de significancia son altos

