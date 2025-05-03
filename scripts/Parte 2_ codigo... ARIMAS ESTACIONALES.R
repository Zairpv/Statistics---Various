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

#Quitar los NA para futuros problemas
Diario_cierre_sinNA=na.remove(Diario_cierre)
Diario_cierre_sinNA #Salio bien la remoción de NA


#-------------------------------------------------------------------
### 3. Analisis descriptivo
#-------------------------------------------------------------------
summary(Anual_cierre)
summary(Mensual_cierre)
summary(Diario_cierre)
library(fBasics)
basicStats(Anual_cierre)
basicStats(Mensual_cierre)

basicStats(Diario_cierre)


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
library(ggfortify)

autoplot(Anual_cierre,xlab = "Año",ylab = "Precio de cierre",main = "Serie anual")
autoplot(Mensual_cierre,xlab = "Año",ylab = "Precio de cierre",main = "Serie mensual")
autoplot(Diario_cierre,xlab = "Año",ylab = "Precio de cierre",main = "Serie diaria")

#Nota: Como puedes observar, todas las series son similares en los patrones temporales. Por lo que solo podrias trabajar con una de ellas. Aparentemente la mejor es la mensual xk la diaria tiene variaciones por los NA 
#Nota. Las series muestran estacionalidad y variaciones importantes a traves del tiempo por lo que podrian no ser estacionarias

#Normalidad
library(car)
qqPlot(Anual_cierre,distribution = "norm",ylab = "Precio de cierre") #Si es normal
qqPlot(Mensual_cierre,distribution = "norm",ylab = "Precio de cierre") #No es normal
qqPlot(Diario_cierre,distribution = "norm",ylab = "Precio de cierre") #No es normal


#-------------------------------------------------------------------
### 4. Prueba de normalidad
#-------------------------------------------------------------------
#Anual
jarque.bera.test(Anual_cierre) #La serie sigue una distribución normal

#Mensual
jarque.bera.test(Mensual_cierre) #La serie no es normal

#Diario
jarque.bera.test(Diario_cierre_sinNA) #No es normal

#Conclusión: las series mensual y diaria deben transformarse

#-------------------------------------------------------------------
### 5. Transformación mensual y diaria
#-------------------------------------------------------------------

#Mensual
jarque.bera.test(sqrt(Mensual_cierre)) #La serie es normal con transformación raiz cuadrada
qqPlot(Mensual_cierre) #original
qqPlot(sqrt(Mensual_cierre)) #transformada -> La serie apenas alcanza la normalidad

#Diaria
jarque.bera.test((Diario_cierre_sinNA)^2) #La serie no es normal
jarque.bera.test(log(Diario_cierre_sinNA)) #La serie no es normal
jarque.bera.test(sqrt(Diario_cierre_sinNA)) #La serie no es normal
#Y si probamos una transf BoxCox (coN la libreria forecast)
library(forecast)
?BoxCox.lambda
(lambda1_diario=BoxCox.lambda(Diario_cierre_sinNA,method = "loglik")) #tarda en correr por la cantidad de datos
(diario_cierre_sinNA_BC=BoxCox(Diario_cierre_sinNA,lambda = lambda1_diario))
jarque.bera.test(diario_cierre_sinNA_BC) #No Se alcanza la normalidad con el método loglike
qqPlot(diario_cierre_sinNA_BC,dist="norm",ylab = "Precio de cierre ($)",main = "Transformación BoxCox1")

(lambda1_diario=BoxCox.lambda(Diario_cierre_sinNA,method = "guerrero")) #tarda en correr por la cantidad de datos
(diario_cierre_sinNA_BC=BoxCox(Diario_cierre_sinNA,lambda = lambda1_diario))
jarque.bera.test(diario_cierre_sinNA_BC) #No Se alcanza la normalidad con el método loglike
qqPlot(diario_cierre_sinNA_BC,dist="norm",ylab = "Precio de cierre ($)",main = "Transformación BoxCox1")
#No se alcanza la transformacion!!!
#Investigar si se pueden otras.... 

#Veamos si con la serie completa se puede
(lambda1_diario=BoxCox.lambda(Diario_cierre,method = "loglik")) #tarda en correr por la cantidad de datos
(diario_cierre_BC=BoxCox(Diario_cierre,lambda = lambda1_diario))
#Quitar NA a esta serie transformada
Diario_cierre_BC_sinNA=na.remove(diario_cierre_BC)
Diario_cierre_BC_sinNA #Salio bien la remoción de NA
jarque.bera.test(diario_cierre_sinNA_BC) #No Se alcanza la normalidad 
qqPlot(diario_cierre_sinNA_BC,dist="norm",ylab = "Precio de cierre ($)",main = "Transformación BoxCox1")
#Tampoco.... Esta serie no es recomendable
#Veamos si con un periodo más corto se puede

diario_cierre_2000=window(Diario_cierre, start = 2000)
Diario_cierre_2000_sinNA=na.remove(diario_cierre_2000)
Diario_cierre_2000_sinNA #Salio bien la remoción de NA
autoplot(diario_cierre_2000)
jarque.bera.test(Diario_cierre_2000_sinNA)#tampoco es normal
jarque.bera.test(sqrt(Diario_cierre_2000_sinNA))#tampoco es normal
jarque.bera.test(log(Diario_cierre_2000_sinNA))#tampoco es normal
jarque.bera.test(exp(Diario_cierre_2000_sinNA))#tampoco es normal
jarque.bera.test((Diario_cierre_2000_sinNA)^2)#tampoco es normal

(lambda1_diario_2000=BoxCox.lambda(Diario_cierre_2000_sinNA,method = "loglik")) #tarda en correr por la cantidad de datos
(diario_cierre_2000BC=BoxCox(Diario_cierre_2000_sinNA,lambda = lambda1_diario_2000))
jarque.bera.test(diario_cierre_2000BC) #No Se alcanza la normalidad con el método loglike
qqPlot(diario_cierre_2000,dist="norm",ylab = "Precio de cierre ($)",main = "Transformación BoxCox1")
#Los datos aparentemente no pueden ser normales... talvez intentando rellenar los valores de fin de semana podria... 

#De momento tendremos que trabajar la serie mensual



#-------------------------------------------------------------------
### 6. Identificación de la señal y nivel de la serie
#-------------------------------------------------------------------
Mensual_cierre_sqrt=sqrt(Mensual_cierre) #definir serie transformada
#Suavizamiento
tt_mensual <- 1:length(Mensual_cierre_sqrt) 
fit_mensual <- ts(loess(Mensual_cierre_sqrt ~ tt_mensual, span = .2)$fitted, start = 1980, frequency = 12)
plot.ts(Mensual_cierre_sqrt, type='l')
grid() 
lines(fit_mensual, col = "red")

#estimación del nivel de la serie (y=b0+e)
fit_level_mensual<-lm(Mensual_cierre_sqrt~1) 
summary(fit_level_mensual) #es significativo
plot.ts(Mensual_cierre_sqrt,main="Nivel de la serie mensual")
lines(ts(fitted(fit_level_mensual),start = 1980,frequency = 12),col="red") #nivel de la serie
#recuerda que el eje Y esta como raiz cuadrada del precio original
lines(fit_mensual, col = "blue") #señal de la serie


#-------------------------------------------------------------------
### 7. Identificación de cambios estructurales de nivel
#-------------------------------------------------------------------
library(strucchange)
mensual_breaks_level=breakpoints(Mensual_cierre_sqrt~1,h=0.1)
summary(mensual_breaks_level) #6 breaks -> 6 cambios de nivel
plot(mensual_breaks_level)
breakdates(mensual_breaks_level)
plot(Mensual_cierre_sqrt,col="dark gray")
lines(fitted(mensual_breaks_level,breaks = 6),col=2)
lines(confint(mensual_breaks_level,breaks = 6),col = 2)
coef(mensual_breaks_level,breaks = 6)

#¿deberia ser igual que en la serie original?
mensual_breaks_level_orig=breakpoints(Mensual_cierre~1,h=0.1)
summary(mensual_breaks_level_orig) #6 breaks #si es igual entonces seguimos trabajando con la serie transformada
plot(mensual_breaks_level_orig) #es igual


#-------------------------------------------------------------------
### 8. Identificación de cambios estructurales de tendencia
#-------------------------------------------------------------------
mensual_breaks_trend_fit=lm(Mensual_cierre_sqrt~tt_mensual)
summary(mensual_breaks_trend_fit) #son significativos el intercepto y la tendencia
plot(Mensual_cierre_sqrt)
lines(ts(fitted(mensual_breaks_trend_fit),start = 1980,frequency = 12),col=2)
#tendencia ascendente - esto indica que no es estacionaria en media

#obtencion de breakpoints
mensual_breaks_trend=breakpoints(Mensual_cierre_sqrt~tt_mensual,h=0.1)
summary(mensual_breaks_trend) #8 breaks - cambios estructurales de tendencia
plot(mensual_breaks_trend)
breakdates(mensual_breaks_trend,breaks = 8)
coef(mensual_breaks_trend,breaks = 8)
plot(Mensual_cierre_sqrt,col="dark gray")
lines(fitted(mensual_breaks_trend,breaks = 8),col=2)
lines(confint(mensual_breaks_trend,breaks = 8)) #Si se represento bien la tendencia a diferencia del codigo previo que no mostraba bien el cambio cuando no estaba transf la serie


#-------------------------------------------------------------------
### 9. Estacionariedad
#-------------------------------------------------------------------
#Supuesto de estacionariedad
#Ho: la serie es no estacionaria
#hA: la serie es estacionaria: no tiene raiz unitaria

adf.test(Mensual_cierre) #Rechaza Ho. Por lo tanto la serie es estacionaria.
adf.test(Mensual_cierre_sqrt) #La serie es NO ESTACIONARIA: tiene raiz unitaria


#Otra forma es con esta funcion: 
library(fUnitRoots)
?adf.test() #permite especificar el numero de lags en la serie e incluir el tipo de regresion de raiz unitaria
#debido a la tendencia escogeremos un tipo ct
adfTest(Mensual_cierre,lags=0,type="ct") #Serie estacionaria
adfTest(Mensual_cierre_sqrt,lags=0,type="ct") #Serie no estacionaria
#Se reafirma la no estacionariedad de la serie de tiempo con pvalue > 0.05


#-------------------------------------------------------------------
### 10. Autocorrelación acf pacf
#-------------------------------------------------------------------
acf(Mensual_cierre) #la autocorrelacion disminuye a medida q aumentan los retrasos, lo que confirma que no hay asociacion lineal entre observaciones seprardas por retrasos mas largos . serie no estacionaria aunq la prueba dickey fuller la considera por poquito como estacionaria
acf(Mensual_cierre_sqrt) #serie no estacionaria

pacf(Mensual_cierre)
pacf(Mensual_cierre_sqrt)

library(astsa)
acf2(Mensual_cierre) #decaimiento lento
acf2(Mensual_cierre_sqrt)

#PARA CUMPLIR EL SUPUESTO DE ESTACIONARIEDAD HAY QUE ELIMINAR TENDENCIA Y ESTACIONALIDAD
componentes_mensual=decompose(Mensual_cierre_sqrt)
plot(componentes_mensual)
#El ruido del componente aleatorio es mayor en los picos donde hay incrementos en el precio

#Cuantas veces debemos diferenciar la serie?
ndiffs(Mensual_cierre_sqrt) #d=1
nsdiffs(Mensual_cierre_sqrt) #No hay diferenciacion por estacionalidad

#definir la serie diferenciada con un lag
Mensual_cierre_sqrt_d1=diff(Mensual_cierre_sqrt,lag = 1)

#comparar ambas series, original y diferenciada
both_mensual=cbind(Mensual_cierre_sqrt,Mensual_cierre_sqrt_d1)
head(both_mensual)
plot(both_mensual)
autoplot(both_mensual,ylab = "Precio de cierre - raiz cuadrada")

#Verificar estacionariedad
plot(Mensual_cierre_sqrt_d1)
acf2(Mensual_cierre_sqrt_d1) #Listo! 
adf.test(Mensual_cierre_sqrt_d1) #La serie es estacionaria!

#Gráficas
par(mfrow=c(2,2))
acf(Mensual_cierre_sqrt,main="ACF - \nSerie transformada")
pacf(Mensual_cierre_sqrt,main="PACF -\n Serie transformada")
acf(Mensual_cierre_sqrt_d1,main="ACF - \nSerie transformada y \ndiferenciada (d=1)")
pacf(Mensual_cierre_sqrt_d1,main="PACF - \nSerie transformada y \ndiferenciada (d=1)")
dev.off()


#-------------------------------------------------------------------
### 11 ARIMAS
#-------------------------------------------------------------------

#se proponen los siguientes modelos ARIMA 
#Arima 1: 1 1 0
#Arima 2: 1 1 1
#Arima 3: 0 1 1
#Arima 4: 2 1 0
#Arima 5: 2 1 2
#Arima 6: 0 1 2

arima1=arima(Mensual_cierre_sqrt,c(1,1,0))
arima2=arima(Mensual_cierre_sqrt,c(1,1,1))
arima3=arima(Mensual_cierre_sqrt,c(0,1,1))
arima4=arima(Mensual_cierre_sqrt,c(2,1,0))
arima5=arima(Mensual_cierre_sqrt,c(2,1,2))
arima6=arima(Mensual_cierre_sqrt,c(0,1,2))

BIC(arima1,arima2,arima3,arima4,arima5,arima6)

#y USANDO UNA FUNCION AUTOMATICA
auto.arima(Mensual_cierre_sqrt,stepwise = T,approximation = F)
#incorporo estacionalidad
?arima
arima7=arima(Mensual_cierre_sqrt,order = c(2,1,0),seasonal = c(1,0,0))
arima7
summary(arima7)

BIC(arima1,arima2,arima3,arima4,arima5,arima6,arima7)


#-------------------------------------------------------------------
### 12. DIAGNOSTICO 
#-------------------------------------------------------------------

#diagnostico de modelo arima 1 y arima 7
library(ggfortify)

ggtsdiag(arima1) #presentan los residuales ruido blanco con media cero, 
#sin picos significativos en acf y pvalues mayores a la significancia, lo que indica que los residuales son independientes
ggtsdiag(arima7) #la misma conclusion que el anterior
#ambos modelos son validos con este criterio

#prueba de box-pierce y ljung box para independencia de residuales
#ho los residuales son independientes
Box.test(arima1$residuals) #si son independientes
Box.test(arima1$residuals,type = "Ljung-Box") #son independientes
jarque.bera.test(arima1$residuals) #no son normales
jarque.bera.test(arima7$residuals) #no son normales
hist(arima1$residuals) 
hist(arima7$residuals)

#PROBAR MAS COMBINACIONES O DIFERENCIACIONES

#-------------------------------------------------------------------
### 13. PRONOSTICO
#-------------------------------------------------------------------
pred_arima1=forecast(arima1,level = c(95),h=5)
autoplot(pred_arima1)
pred_arima1=forecast(arima1,level = c(95),h=20)
autoplot(pred_arima1)

pred_arima7=forecast(arima7,level = c(95),h=5)
autoplot(pred_arima7)
pred_arima7=forecast(arima7,level = c(95),h=20)
autoplot(pred_arima7)
pred_arima7=forecast(arima7,level = c(95),h=30)
autoplot(pred_arima7)
#Sus predicciones no estan correctas #hay q buscar una combinación que si cumpla normalidad.
#este modelo no es tan adecuado


#-------------------------------------------------------------------
### 14 PROBANDO CON UN MODELO AR A LA SERIE DIFERENCIADA
#-------------------------------------------------------------------
plot(Mensual_cierre_sqrt_d1) #Esta serie no presenta tendencia
acf2(Mensual_cierre_sqrt_d1)
mod1=ar(Mensual_cierre_sqrt_d1,method="mle")
mod1$order #fue 5 #nos dice el orden de los picos significativos

#Prueba ADF ajustada a 5 lags
#Ho: la serie es no estacionaria. Tiene raiz unitaria
adfTest(Mensual_cierre_sqrt,lags = 5,type = "c") #serie estacionaria

#Hay pendiente? cuánto vale la constante?
t=seq(2:length(Mensual_cierre_sqrt))
mod2=lm(Mensual_cierre_sqrt_d1~t)
summary(mod2) #ni el intercepto ni la pendiente son significativos
#no hay deriva

#Prueba adf ajustada sin constante
adfTest(Mensual_cierre_sqrt,lags = 5,type = "nc") #la serie tiene raiz unitaria
#Requiere diferenciación
#comprobemos
ndiffs(Mensual_cierre_sqrt)
ndiffs(Mensual_cierre) #Solo una diferenciacion d=1

#Probando con unitrootTest
unitrootTest(Mensual_cierre_sqrt,lags = 5,type = "nc")
#Obtuvo la misma conclusión con cinco lags, veamos si con uno tambien:
unitrootTest(Mensual_cierre_sqrt,lags = 1,type = "nc")
#pOR LO TANTO, se concluye que la serie tiene raiz unitaria y requiere una diferenciacion


#-------------------------------------------------------------------
### 15 ELIMINANDO ESTACIONARIEDAD Y ESTACIONALIDAD
#-------------------------------------------------------------------
autoplot(Mensual_cierre_sqrt_d1,main = "serie mensual transformada") #la serie diferenciada ya no tiene estacionariedad

#PARA AJUSTAR EL MODELO ARIMA HAY QUE ELIMINAR LA NO ESTACIONARIEDAD MEDIANTE UNA DIFERENCIACION. 
#COMO LA SERIE TIENE ESTACIONALIDAD, ESTE COMPONENTE DEBE ELIMINARSE TAMBIEN. Y POSTERIORMENTE SE DIFERENCIA PARA QUITAR ESTACIONARIEDAD

#sin estacionalidad
serie_mensual_ajustada=Mensual_cierre_sqrt-componentes_mensual$seasonal
autoplot(serie_mensual_ajustada,main = "serie mensual transformada sin estacionalidad")

serie_mensual_estacionaria_d1=diff(serie_mensual_ajustada,differences = 1)
autoplot(serie_mensual_estacionaria_d1,main = "serie estacionaria y sin estacionalidad  (d=1)")

#-------------------------------------------------------------------
### 16 IDENTIFICANDO VALORES P D Q
#-------------------------------------------------------------------
acf2(serie_mensual_estacionaria_d1)
frequency(Mensual_cierre_sqrt)

#Se proponen los siguientes modelos arima estacionales
#arima1: arima (1,1,1)(1,0,0)(12)
Arima1<- arima(Mensual_cierre_sqrt, order=c(1,1,1),seasonal = list(order = c(1,0,0), period = 12),method="ML") 
library(lmtest)
coeftest(Arima1)
#arima2: arima (1,1,1)(1,1,0)(12)
Arima2<- arima(Mensual_cierre_sqrt, order=c(1,1,1),seasonal = list(order = c(1,1,0), period = 12),method="ML") 
coeftest(Arima2)
#arima3: arima (1,1,1)(1,1,1)(12)
Arima3<- arima(Mensual_cierre_sqrt, order=c(1,1,1),seasonal = list(order = c(1,1,1), period = 12),method="ML") 
coeftest(Arima3)
#arima4: arima (1,1,1)(1,2,1)(12)
Arima4<- arima(Mensual_cierre_sqrt, order=c(1,1,1),seasonal = list(order = c(1,2,1), period = 12),method="ML") 
coeftest(Arima4)
#arima5: arima (1,2,1)(1,2,1)(12)
Arima5<- arima(Mensual_cierre_sqrt, order=c(1,2,1),seasonal = list(order = c(1,2,1), period = 12),method="ML") 
coeftest(Arima5)
#arima6: arima (0,2,1)(1,2,1)(12)
Arima6<- arima(Mensual_cierre_sqrt, order=c(0,2,1),seasonal = list(order = c(1,2,1), period = 12),method="ML") 
coeftest(Arima6) #Todos son significativos

AIC(Arima1,Arima2,Arima3,Arima4,Arima5,Arima6)
#Aparentemente el modelo 3 y el 6 

#Y automatico=
auto.arima(Mensual_cierre_sqrt,stepwise = FALSE,seasonal = TRUE)
Arima7<- arima(Mensual_cierre_sqrt, order=c(2,1,0),seasonal = list(order = c(2,0,0), period = 12),method="ML") 
coeftest(Arima7) #nO son significativos

#-------------------------------------------------------------------
### 16 diagnostico
#-------------------------------------------------------------------
ggtsdiag(Arima3)
ggtsdiag(Arima6)
ggtsdiag(Arima7) #parece ser el mejor

Box.test(arima3$residuals,type = "Ljung-Box")
Box.test(arima6$residuals,type = "Ljung-Box")
Box.test(arima7$residuals,type = "Ljung-Box") #todos son residuales independientes


jarque.bera.test(Arima3$residuals)
jarque.bera.test(Arima6$residuals)
jarque.bera.test(Arima7$residuals)
jarque.bera.test(Arima1$residuals)
jarque.bera.test(Arima2$residuals)
jarque.bera.test(Arima4$residuals)
jarque.bera.test(Arima5$residuals)
#Aun no cumplen normalidad 
#Buscar otras combinaciones para ver si se logra encontrar un modelo con normalidad

hist(Arima3$residuals)
qqPlot(Arima3$residuals,distribution = "norm")
shapiro.test(Arima3$residuals)

hist(Arima6$residuals)
qqPlot(Arima6$residuals,distribution = "norm") #Este modelo es mas cercano a la normalidad
shapiro.test(Arima6$residuals)

hist(Arima7$residuals)
qqPlot(Arima7$residuals,distribution = "norm") 
shapiro.test(Arima7$residuals)


#-------------------------------------------------------------------
### 17 diagnostico
#-------------------------------------------------------------------

#tomaremos solo Arima 6 porque fue el que casi alcanzo normalidad
pred_Arima6=forecast(Arima6,h=10,level = 99.5)
autoplot(pred_Arima6) #WOOW se ve la diferencia en los modelos arima anteriores

pred_Arima6=forecast(Arima6,h=20,level = 99.5)
autoplot(pred_Arima6) #WOOW se ve la diferencia en los modelos arima anteriores

pred_Arima6=forecast(Arima6,h=30,level = 99.5)
autoplot(pred_Arima6) #WOOW se ve la diferencia en los modelos arima anteriores

pred_Arima6=forecast(Arima6,h=50,level = 99.5)
autoplot(pred_Arima6) #WOOW se ve la diferencia en los modelos arima anteriores
#Este modelo parece ser el más optimo 
#verifica que tanto importa la normalidad en un modelo arima estacional

#Checa mi tarea 6. Igual ahi puedes definir una serie de entrenamiento y prueba para sacar valores de error
#El codigo es el mismo

#-------------------------------------------------------------------
### 18 Incorporando otra variable X
#-------------------------------------------------------------------

DATS_PETROLEO <- read.delim("~/08. R scripts/Series - Precios cafe/DATS_PETROLEO.txt")
View(DATS_PETROLEO)

Petroleo_Mensual_cierre<-ts(DATS_PETROLEO$Cierre_petroleo,start = c(1984,1),end = c(2022,12),frequency = 12)
Petroleo_Mensual_cierre

#Hacer una ventana de precio cafe que coincida con el mismo periodo que petroleo
Mensual_cierre_sqrt_1984=window(Mensual_cierre_sqrt, start = 1984)


#Correlacion entre variables
basicStats(Petroleo_Mensual_cierre)
autoplot(Petroleo_Mensual_cierre,xlab = "Año",ylab = "Precio de cierre",main = "Serie mensual - petroleo")
ccf(diff(Petroleo_Mensual_cierre),diff(Mensual_cierre_sqrt_1984),ylab="Cross correlation",type = "correlation")
#la correlacion esta dada por retrasos en la serie x
#la funcion ccf permite identificar lags utiles para predecir y
#se toman la primer diferenciacion para evitar una regresion espuria
#asi ambas series son estacionarias
#su mayor relación es el el lag -1.5 aprox esto puede indicar que x conduce a y
grangertest(diff(Petroleo_Mensual_cierre),diff(Mensual_cierre_sqrt_1984),order = 3)
#ho la serie x no causa a serie Y
#ha la serie de tiempo x si es causal de Y
#Ups no se rechaza Ho, indica que no hay relacion causal

#-------------------------------------------------------------------
### 18 analisis de datos atipicos
#-------------------------------------------------------------------
bp=boxplot(Mensual_cierre_sqrt)
bp$out
library(tsoutliers)
(outliers_mensual_sqrt=tso(Mensual_cierre_sqrt))
plot(outliers_mensual_sqrt)


#Como el precio del petroleo no es causal del precio de cafe no se puede incorporar una variable de intervencion
#ya que esta se hace para la variable x...


#-------------------------------------------------------------------
### 19. MODELO ARCH
#-------------------------------------------------------------------
#MODELO ARCH

#checamos errores del modelo 6 ARIMA ESTACIONAL
errores_Arima6_cuadrado=resid(Arima6)^2
library(quantmod)
chartSeries(errores_Arima6_cuadrado)
#Observamos que nuestra varianza no es constante, ya que los errores al cuadrado muestran que al pasar los días la varianza llega a ser heterocedastica, por lo que ahora haremos nuestra regresión. 
library(dynlm)
regresion1=dynlm(errores_Arima6_cuadrado~L(errores_Arima6_cuadrado))
summary(regresion1) #son significativos
#H0: No hay efectos ARCH si el p-value es mayor que 0.05
#H1: Sí hay efectos ARCH si el p-value es menor que 0.05
#Conclusión: Sí hay efectos ARCH con un rezago
#Observamos claramente que el intercepto y nuestra pendiente llegan a ser significativos, rechazamos la Hipótesis nula y concluimos que sí hay efectos ARCH dentro de nuestra varianza de los errores al cuadrado a un rezago.
#https://rpubs.com/AlbertoMadinRivera/710259

#vERIFICAR Autocorrelación y Autocorrelación Parcial de nuestro modelo, para comprobar que nuestra varianza es Heterocedástica:
autoplot(acf(errores_Arima6_cuadrado, lag.max = 2023, ylim = c(-0.5,1))) +
  labs(title = "Autocorrelación de los errores al cuadrado") + 
  xlab("Rezagos") +
  ylab("Autocorrelación") #eL 2023 LO ENCUENTRAS EN EL SUMMARY DE REGRESION1 en donde dice "end"

autoplot(Pacf(errores_Arima6_cuadrado, lag.max = 2023, ylim = c(-0.5,1))) +
  labs(title = "Autocorrelación de los errores al cuadrado") + 
  xlab("Rezagos") +
  ylab("Autocorrelación") 

##Tanto en la autocorrelación y autocorrelación parcial llegan a salir del límite de criterio, podemos decir que hay ruido blanco, en otras palabras: hay Heterocedacistidad en la varianza del modelo Arima 6 estacional

#Prueba ARCH para verificar
library(FinTS)
ModeloARCH1 = ArchTest(Mensual_cierre_sqrt, lags = 1, demean = TRUE)

#Revisamos
ModeloARCH1
#H0: No hay efectos ARCH si el p-value es mayor que 0.05
#H1: Sí hay efectos ARCH si el p-value es menor que 0.05
#Conclusión: Sí hay efectos ARCH con un rezago

#Existe volatilidad al primer rezago

#Verificamos que nuestro modelo cumpla las condiciones para ser un modelo ARCH a través del p-value. Concluimos que llega a ser Heterocedástico.

#-------------------------------------------------------------------
### 20. MODELO GARCH
#-------------------------------------------------------------------
#Para comprobar que se trata de un modelo ARCH generaremos un modelo GARCH. 
#Siendo la condición de que hay uno o más puntos de datos en una serie para los cuales la varianza del término de error actual o 
#innovación es una función de los tamaños reales de los términos de error de los períodos de tiempo anteriores: se relaciona con los cuadrados de las innovaciones anteriores. 
#En la econometría, los modelos ARCH se utilizan para caracterizar y modelar series temporales. Una variedad de otras siglas se aplican a las estructuras particulares que tienen una base similar.

#Modelo ARMA (1,1)
library(rugarch)
ugarch1 = ugarchspec()

#Resumen
ugarch1


#Modelo ARMA (2,1) <--- Modelo efectivo
ugarch2 = ugarchspec(mean.model = list(armaOrder = c(2,1)))

ugarch2
#ESTO ANTERIOR FUNCIONA COMO SI PRIMERO HICIERAMOS CAJAS PREDETERMINADAS DONDE DEPOSITAREMOS LAS PREDICCIONES PARA UNA SERIE ESPECIFICA

ugfit = ugarchfit(spec = ugarch2, data = Mensual_cierre_sqrt)

ugfit
#Alpha1: coeficiente de los residuales al cuadrado
#Beta1: coeficiente de la varianza al cuadrado con un rezago

#Observamos el valor de nuestros coeficientes
ugfit@fit$coef
#Imprimir varianza
ug_var = ugfit@fit$var

autoplot(ts(ug_var)) + 
  geom_line(color = "darkgreen") + labs(title = "Varianza de nuestro modelo ARMA (2,1)")
#Residuales
ug_resid = (ugfit@fit$residuals)^2

autoplot(ts(ug_resid)) + autolayer(ts(ug_resid), series = "Residuales al cuadrado") + autolayer(ts(ug_var), series = "Varianza") + labs(title = "Residuales al cuadrado y Varianza de nuestro modelo ARMA (2,1)") + ylab("") + xlab("Tiempo")

#Pronóstico

#Por último observamos cuáles serán nuestros valores pronosticados a un mes despues del rendimiento de nuestras acciones de la siguiente forma.

ug_forecast = ugarchforecast(ugfit, n.ahead = 30)

ug_forecast
