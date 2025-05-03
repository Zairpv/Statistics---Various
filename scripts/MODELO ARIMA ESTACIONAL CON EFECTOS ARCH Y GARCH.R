rm(list=ls()) 
options(max.print = 1000000000) 

library(tseries)
library(ggplot2)
library(FinTS)
library(rugarch)
library(dynlm)
library(vars)
library(nlWaldTest)
library(lmtest)
library(broom)
library(car)
library(sandwich)
library(knitr)
library(forecast)
library(pdfetch)
library(tsbox)
library(stats)
library(zoo)
library(vrtest)
library(ggfortify)


datos_mensual<-read.csv("Cafe_mensual.csv",header=TRUE) 
View(datos_mensual)

#-------------------------------------------------------------------
###2. Serie de tiempo
#-------------------------------------------------------------------
##Mensual
Mensual_cierre<-ts(datos_mensual$cierre,start = c(1980,1),end = c(2023,01),frequency = 12)
Mensual_cierre

#Plots
plot.ts(Mensual_cierre) 
#Se observa que hay varianza diferente en distintos periodos de tiempo
hist(Mensual_cierre,freq = F) #aparentemente no es normal la serie
plot.ts(diff(Mensual_cierre),ylab="Cambio en Precio") #Los picos que se muestran hacen pensar que requieren un modelo arch - garch
#tienen mucho ruido: los cambios en la varianza son en realidad una función de las varianzas pasadas o 
title("Primera diferencia de precio")


#Normalidad
adf.test(Mensual_cierre) #No es normal
adf.test(sqrt(Mensual_cierre)) #Con transformación logaritmica ya es normal
sqrt_cierre=sqrt(Mensual_cierre)
hist(sqrt_cierre,frecuency=F)
plot.ts(sqrt_cierre)
title("Serie de tiempo de la raiz cuadrada de precio")


#Estacionaridad de varianza y media
?Auto.VR
Auto.VR(sqrt_cierre) #El valor stats es alto: significa que la varianza no es constante
adf.test(sqrt_cierre,k=1) #Esta evalua si se conserva la normalidad con un lag (1 diferenciacion)
adf.test(sqrt_cierre,k=2) #Esta evalua si se conserva la normalidad con dos diferenciaciones
#Por lo tanto los datos no son estacionarios y requiere dos diferenciaciones para que lo sea

#Trabajaremos con una diferenciación
?diff
sqrt_cierre_d2=diff(sqrt_cierre,differences = 2)

# Determinar AR y MA componentes
#Utilizamos la cierre diferenciada
acf(sqrt_cierre_d2, main="ACF",lag.max = 50) 
#Ma puede tomar (1)
pacf(sqrt_cierre_d2,main="PACF",lag.max=50)
#Ar puede tomar el valor de 1, 2, 3,4,5,6,7 (son siete lags consecutivos fuera del umbral)

#ARIMA p d q (AR dIF Ma)

#Antes de probar los anteriores hagamos un autoarima
auto.arima(log_cierre_d1) #Da (0,0,2) (0,0,1)(12) #Este es estacional
#Pero como ya tiene 2 diferenciaciones, seria (0,2,2)(0,0,1)(12)

#Arima #Ya tomamos la original y ponemos una diferenciacion y con dos diferenciaciones
arima_estacional_d1=arima(sqrt_cierre,order = c(0,1,2),seasonal = list(order = c(0,1,1),period=12),method = "ML")
summary(arima_estacional_d1)
coeftest(arima_estacional_d1)
arima_estacional_d2=arima(sqrt_cierre,order = c(0,2,1),seasonal = list(order = c(1,2,1),period=12),method = "ML")
summary(arima_estacional_d2)
coeftest(arima_estacional_d2)
arima_estacional_d3=arima(sqrt_cierre,order = c(0,2,1),seasonal = list(order = c(1,1,1),period=12),method = "ML")
summary(arima_estacional_d3)
coeftest(arima_estacional_d3)

AIC(arima_estacional_d1,arima_estacional_d2,arima_estacional_d3) #EL MODELO D1 ES EL MEJOR
BIC(arima_estacional_d1,arima_estacional_d2,arima_estacional_d3) 

#Diagnostico
ggtsdiag(arima_estacional_d3)
ggtsdiag(arima_estacional_d2)
ggtsdiag(arima_estacional_d1) #Este es el mejor aparentemente
checkresiduals(arima_estacional_d1)
autoplot(arima_estacional_d1) #En rojo es lo que predice ARIMA modelo y en negro la serie real,... se ve muy bien

arima_estacional_d1_residuales=arima_estacional_d1$residuals
ggtsdisplay(arima_estacional_d1_residuales,main="Precios ARIMA(0,1,2)(0,1,1)(12) Residuales")
#Casi no hay volatilidad

#Usaremos estos residuales para ver los garch

#Estimar la ecuacion media r = beta + error
precio_residual_mean=dynlm(arima_estacional_d1_residuales~1)
summary(precio_residual_mean)#Aqui si se espera que el pvalue no sea significativo
#Porque indica que hay aun volatilidad en la varianza...  se puede usar el garch
#Pvalue > 0.05 indica que no hay cambios en la media a traves del tiempo

###Determinación del efecto ARCH
ehatsq=ts(resid(precio_residual_mean)^2)
effect_arch_resid=dynlm(ehatsq~L(ehatsq))
summary(effect_arch_resid) #Son significativos! significa que el cuadrado de los residuos pasados explica la escala actual de los residuos presentes 
#es decir, los residuos son una funcion de su pasado y esto se conoce como volatilidad
#ESTO APRUEBA COMPLETAMENTE EL MODELO

#para verificar el efecto se usa una prueba de chi-cuadrada
t=nobs(precio_residual_mean)
q=length(coef(effect_arch_resid))-1
rsq=glance(effect_arch_resid)[[1]]
lm=(t-q)*rsq
alpha=0.05
chicr=qchisq(1-alpha,q)
lm
chicr #Es grande el valor entonces valida que hay efecto arch

#Esta es otra forma de validarlo
precio_archeff1=ArchTest(arima_estacional_d1_residuales,lags = 1,demean = T)
precio_archeff1
#Hipotesis Ho: no hay efecto arch
#sE rechaza H0.... si hay efecto arch --- es decir, volatilidad condicional a los residuales


#Estimando ahora la ecuacion garch
#el primer item en c() es efecto garch y el segundo es efecto arch
#Evaluamos con un efecto 1 en arch
#iteracion de 500 veces
precio_arch=garch(arima_estacional_d1_residuales,c(0,1),control = garch.control(maxiter = 500,grad = "numerical"))
summary(precio_arch) #Hay significancia!
#tambien el test Box-Ljung no es significativo _ hay independencia de residuales

#Con un efecto garch
precio_arch2=garch(arima_estacional_d1_residuales,c(1,1),control = garch.control(maxiter = 500,grad = "numerical"))
summary(precio_arch2) #Hay significancia!
#tambien el test Box-Ljung no es significativo _ hay independencia de residuales
#LLegamos a un modelo apropiado

AIC(precio_arch,precio_arch2) #Es mejor con un efecto garch! el segundo

#Estimemos volatilidad
hhat=ts(2*precio_arch2$fitted.values[-1,1]^2)
plot.ts(hhat)
title("volatilidad del precio de cierre de café")
#muestra la parte inestable de los datos
#los picos indican la parte que es impredecible o donde hubo un cambio desconocido 

#-------------------------------------------------------------------
###3. GARCH
#-------------------------------------------------------------------
#El modelo GARCH describe la varianca del termino de error actual siguiendo un modelo ARMA