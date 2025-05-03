library(agricolae)

##### Hoja ACHACHAIRU
setwd("D:/zaira/Documents/")
#(Copiar hoja achachairu en portapeles)
achachairu_data=read.table("clipboard",header = T)
View(achachairu_data)

### 1. ¿Qué variables son categorias?:
#Predios: 2 niveles (predio 1, predio 2)
achachairu_data$PREDIO=as.factor(achachairu_data$PREDIO)
levels(achachairu_data$PREDIO)
#Fechas: 7 niveles (1...7)
achachairu_data$FECHA=as.factor(achachairu_data$FECHA)
levels(achachairu_data$FECHA)


### Nota. La prueba de t verifica si las medias de "DOS" grupos son iguales o no,
#en este caso, solo puede probar si las medias una variable medida en la fecha 1 son diferentes entre el predio 1 y 2.
#(y el procedimiento se repite hasta la fecha 7 y por cada una de las variables de interes que fueron medidas)
#Para hacer una evaluación de medias diferentes en predio + fecha se usa ANOVA de dos vias, que es un método de comparación múltiple). 


### 2. Desagrupar base de datos 
#Para verificación de supuestos debe separarse la base por predios y verificar normalidad.
#También se debe separar la base por fechas para que la prueba de T se pueda realizar.

#2.1. Por predios
achachairu_data_predio1=achachairu_data[1:56,]
View(achachairu_data_predio1)
achachairu_data_predio2=achachairu_data[57:112,]
View(achachairu_data_predio2)
#56 datos por predio

#2.2. Por fechas
achachairu_data_fecha1=achachairu_data[achachairu_data$FECHA==1,]
View(achachairu_data_fecha1)
#16 datos por fecha: 2 predios por 8 repeticiones en cada fecha
achachairu_data_fecha2=achachairu_data[achachairu_data$FECHA==2,]
View(achachairu_data_fecha2)
achachairu_data_fecha3=achachairu_data[achachairu_data$FECHA==3,]
View(achachairu_data_fecha3)
achachairu_data_fecha4=achachairu_data[achachairu_data$FECHA==4,]
View(achachairu_data_fecha4)
achachairu_data_fecha5=achachairu_data[achachairu_data$FECHA==5,]
View(achachairu_data_fecha5)
achachairu_data_fecha6=achachairu_data[achachairu_data$FECHA==6,]
View(achachairu_data_fecha6)
achachairu_data_fecha7=achachairu_data[achachairu_data$FECHA==7,]
View(achachairu_data_fecha7)


### 3. ¿Cumple supuestos para analisis parametrico t student o anova?
#Supuestos para comparación de medias: normalidad y homogeneidad de varianzas (homocedasticidad )
#a) Normalidad -> Se utiliza Kolmogorov Smirnov al tener mas de 50 datos (pvalue>0.05 significa que hay normalidad)
#Ho: los datos son normales
#Ha: lo contrario
#Rechazar Ho si pvalue < 0.05

library(nortest)
#Para la variable de interes en todo el conjunto de datos:
lillie.test(achachairu_data$PERPESO) #Es normal
#Para la variable de interes en el predio X:
lillie.test(achachairu_data_predio1$PERPESO) #Es normal
lillie.test(achachairu_data_predio2$PERPESO) #Es normal
#Para la variable de interes en la fecha X:
lillie.test(achachairu_data_fecha1$PERPESO) #Es normal
lillie.test(achachairu_data_fecha2$PERPESO) #Es normal
lillie.test(achachairu_data_fecha3$PERPESO) #Es normal
lillie.test(achachairu_data_fecha4$PERPESO) #Es normal
lillie.test(achachairu_data_fecha5$PERPESO) #Es normal
lillie.test(achachairu_data_fecha6$PERPESO) #Es normal
lillie.test(achachairu_data_fecha7$PERPESO) #Es normal


#b) Homocedasticidad -> Exploracion con boxplots pero debe desagruparse por predios
#variable de interes~fecha del predio X:
boxplot(achachairu_data_predio1$PERPESO~achachairu_data_predio1$FECHA)
boxplot(achachairu_data_predio2$PERPESO~achachairu_data_predio2$FECHA)
#->La varianza de pérdida de peso es ligeramente diferente, sobre todo en predio 2 pero en general si hay homocedasticidad

#variable de interes~predio de la fecha X:
boxplot(achachairu_data_fecha1$PERPESO~achachairu_data_fecha1$PREDIO,xlab = "predio",ylab = "pérdida de peso",main="Fecha 1")
boxplot(achachairu_data_fecha2$PERPESO~achachairu_data_fecha2$PREDIO,xlab = "predio",ylab = "pérdida de peso",main="Fecha 2")
boxplot(achachairu_data_fecha3$PERPESO~achachairu_data_fecha3$PREDIO,xlab = "predio",ylab = "pérdida de peso",main="Fecha 3")
boxplot(achachairu_data_fecha4$PERPESO~achachairu_data_fecha4$PREDIO,xlab = "predio",ylab = "pérdida de peso",main="Fecha 4")
boxplot(achachairu_data_fecha5$PERPESO~achachairu_data_fecha5$PREDIO,xlab = "predio",ylab = "pérdida de peso",main="Fecha 5")
boxplot(achachairu_data_fecha6$PERPESO~achachairu_data_fecha6$PREDIO,xlab = "predio",ylab = "pérdida de peso",main="Fecha 6")
boxplot(achachairu_data_fecha7$PERPESO~achachairu_data_fecha7$PREDIO,xlab = "predio",ylab = "pérdida de peso",main="Fecha 7")
#Estos son los boxplots que se reportan. Aparentemente las varianzas son diferentes entre predios de una fecha X


#Verificar: Prueba de homogeneidad de varianzas -> Levene test (pvalue > 0.05 significa que hay homogeneidad de varianzas entre fechas de un predio X)
#Ho: los datos tienen varianzas iguales
#Ha: lo contrario  
#Rechazar Ho si pvalue < 0.05
library(car)
#variable de interes~fecha del predio X:
leveneTest(achachairu_data_predio1$PERPESO~achachairu_data_predio1$FECHA)
leveneTest(achachairu_data_predio2$PERPESO~achachairu_data_predio2$FECHA)
#variable de interes~predio de la fecha X:
leveneTest(achachairu_data_fecha1$PERPESO~achachairu_data_fecha1$PREDIO)
leveneTest(achachairu_data_fecha2$PERPESO~achachairu_data_fecha2$PREDIO)
leveneTest(achachairu_data_fecha3$PERPESO~achachairu_data_fecha3$PREDIO)
leveneTest(achachairu_data_fecha4$PERPESO~achachairu_data_fecha4$PREDIO)
leveneTest(achachairu_data_fecha5$PERPESO~achachairu_data_fecha5$PREDIO)
leveneTest(achachairu_data_fecha6$PERPESO~achachairu_data_fecha6$PREDIO)
leveneTest(achachairu_data_fecha7$PERPESO~achachairu_data_fecha7$PREDIO)


##Conclusión: Se cumplen supuestos parametricos normalidad y homogeneidad de varianzas en la variable pérdida de peso, tanto por predios como por fechas
#------ Nota: Repetir el mismo procedimiento para las variables el resto de las variables medidas --------


##### 4. Prueba de T 
#Se debe realizar para cada uno de las fechas, ya que es una prueba que solo evalua 
#diferencias entre medias de dos grupos (predio 1 y predio 2)

#----Nota: Se debe especificar en la función t.test que la homogeneidad de varianzas esta asegurada y que 
#ademas arroje el intervalo de confianza para corroborar diferencia de medias
t.test(achachairu_data_fecha1$PERPESO~achachairu_data_fecha1$PREDIO,var.eq=TRUE,conf.int=TRUE)

  #---Nota: pvalue < 0.05 es menor al alpha 0.05, con intervalos de confianza entre VALOR 1  a VALOR 2
#En este caso el contraste y prueba de hipotesis es:
#Ho: las medias son iguales
#Ha: lo contrario 

#CONCLUSIÓN: se rechaza la hipotesis Ho, lo que indica que las medias de pérdida de peso medidas
#en la fecha 1 SON SIGNIFICATIVAMENTE DIFERENTES entre el predio 1 y el predio 2, con un intervalo 
#de confianza entre -9.92 y -3.77 y un valor de alpha de 0.05.  

t.test(achachairu_data_fecha2$PERPESO~achachairu_data_fecha2$PREDIO,var.eq=TRUE,conf.int=TRUE)
t.test(achachairu_data_fecha3$PERPESO~achachairu_data_fecha3$PREDIO,var.eq=TRUE,conf.int=TRUE)
t.test(achachairu_data_fecha4$PERPESO~achachairu_data_fecha4$PREDIO,var.eq=TRUE,conf.int=TRUE)
t.test(achachairu_data_fecha5$PERPESO~achachairu_data_fecha5$PREDIO,var.eq=TRUE,conf.int=TRUE)
t.test(achachairu_data_fecha6$PERPESO~achachairu_data_fecha6$PREDIO,var.eq=TRUE,conf.int=TRUE)
t.test(achachairu_data_fecha7$PERPESO~achachairu_data_fecha7$PREDIO,var.eq=TRUE,conf.int=TRUE)
#CONCLUSIÓN: en todas las fechas se rechaza la hipotesis nula, por lo tanto, la media de
#pérdida de peso es diferente entre predios en cada una de las fechas evaluadas.

#Nota. El rango de intervalos de confianza indica que si en ese intervalo se encuentra el valor cero (EN ESTE CASO NO), 
#habria una alta probabilidad de que las medias de la variable de interes SI SEAN IGUALES, (en este caso, el intervalo de confianza de cada fecha
#esta por debajo del valor 0, pero el comportamiento podria ser diferente para otra de las variables medidas)

## ---- Nota. Repetir el procedimiento para el resto de variables medidas------


##### 5. ANOVA GLM (este anova se realiza debido a que son dos variables factoriales, es un anova de dos vias)
?glm #Se debe especificar la distribución de los datos y el tipo de efecto; ADITIVO
mod1_glm=glm(PERPESO~PREDIO+FECHA,data = achachairu_data,family = gaussian)
summary(mod1_glm)
#Hay diferencias entre predios y entre fechas, con excepción de la fecha 2 y 3
#para este tipo de modelos se utiliza la función aov
aov(mod1_glm) 

?glm #Se debe especificar la distribución de los datos y el tipo de efecto; INTERACCION
mod2_glm=glm(PERPESO~PREDIO*FECHA,data = achachairu_data,family = gaussian)
summary(mod2_glm)
#Hay diferencias entre predios y entre fechas, con excepción de la fecha 2 y 3
aov(mod2_glm) #Nota. Dice que los datos no estan balanceados, aov prueba con un anova tipo 1, 
#esto quiere decir que una funcion lm haría lo mismo que un glm

#EXTRA: Graficas de interacción :D
library(phia)
plot(interactionMeans(mod2_glm)) #Esta grafica indica las interacciones entre predios * fecha
testInteractions(mod2_glm) #Esta tabla desglosa la tabla anterior
dev.off()

#Checar residuales para verificar que el modelo fue ajustado correctamente
#y que la interpretacion de la funcion aov es adecuada
residualPlot(mod1_glm) #Lucen bien!!!
residualPlot(mod2_glm) #Lucen bien!!!
res_m1=residuals(mod1_glm)
res_m2=residuals(mod2_glm)
qqnorm(res_m1)  
qqline(res_m1)  
qqnorm(res_m2)  
qqline(res_m2)  
lillie.test(res_m1)
lillie.test(res_m2)
#El modelo aditivo de glm para el anova es mas adecuado porque cumple sus dos supuestos de residuales
#Se confirma: 
AIC(mod1_glm,mod2_glm)

#Se vuelve a crear anovaglm pero con el paquete "car" (este es el que se recomienda reportar)
#Como los datos en el aov anterior indicaban que no estaban balanceados es mejor utilizar anova tipo 3 para reportar
Anova(mod1_glm,type=c("III"))
Anova(mod1_glm,type=c("II")) #(Ambos tipos de anova son iguales, el anova glm esta ajustado correctamente con cumplimiento de residuales y de supuestos :D)


#Nota----En realidad como es un experimento con repeticiones e interacciones predio y fecha
#tambien podria servirle un modelo lineal de efectos mixtos o simplemente un anova para un modelo lineal (libreria nmle)
#debido a q los datos son normales y los glm son mas adecuados a variables con efectos aleatorios que provocan que no se distribuyan en forma gaussiana
#en ese enlace viene mas detalle por si lo necesitan
#https://www.r-bloggers.com/2017/06/linear-models-anova-glms-and-mixed-effects-models-in-r/


##### 6. Tukey test
#A partir de la tabla de resumen, ambos factores tienen un efecto significativo
#pero con solo mirar esto es muy difícil identificar claramente qué niveles son los significativos.
#Por eso es que solo se considera fechas, para identificar las fechas con mayor efecto en las medias
mod2_aov=aov(PERPESO~FECHA,data = achachairu_data)
#TUKEYHSD solo trabaja con modelos aov 
TukeyHSD(mod2_aov,conf.level = 0.95)
#Nota: en esta prueba hay q observar valores de pvalue = 0
#que indican una gran significancia en cada una de las combinaciones



#####7. Prueba de Duncan
#Se utiliza otra vez el modelo anterior
#Prueba de Duncan

#Nota: el Test de Duncan es de comparaciones múltiples. 
#Permite comparar las medias de los t niveles de un factor  después de haber rechazado la Hipótesis nula de igualdad de medias mediante la técnica ANOVA. 

library(agricolae)
?duncan.test()
duncan.test(mod2_aov,"FECHA",console = T,alpha = 0.05)
#Si se le especifica grupos = false entonces desagrupa las fechas pero es recomendable solo reportar la anterior
duncan.test(mod2_aov,"FECHA",console = T,alpha = 0.05,group = FALSE)

