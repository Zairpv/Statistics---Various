#-------------------------------------------------------------
#Codigo K PREPARAR CONSOLA
#-------------------------------------------------------------


rm(list = ls())
datos_FULL=read.table("clipboard",header=T,sep="\t")
View(datos_FULL)
str(datos_FULL)
options(max.print=1000000)
setwd("D:/zaira/Documents/Karen")

#Paso 1. Definir vectores
datos_FULL$edad=as.factor(datos_FULL$edad) #F EDAD
levels(datos_FULL$edad)

datos_FULL$mes=as.factor(datos_FULL$mes) #F MES
levels(datos_FULL$mes)

datos_FULL$LABEL_MES=as.factor(datos_FULL$mes) # Mes Etiqueta 
levels(datos_FULL$mes)

datos_FULL$conglomerado=as.factor(datos_FULL$conglomerado)
levels(datos_FULL$conglomerado)

datos_FULL$ESTACION=as.factor(datos_FULL$ESTACION)
levels(datos_FULL$ESTACION)

#ANOVA PRELIMINAR

#Normalidad
ks.test(datos_FULL$flujo,pnorm,mean(datos_FULL$flujo),sd(datos_FULL$flujo))
hist(datos_FULL$flujo)

ks.test(log(datos_FULL$flujo),pnorm,mean(log(datos_FULL$flujo)),sd(log(datos_FULL$flujo)))
hist(log(datos_FULL$flujo))

ks.test(sqrt(datos_FULL$flujo),pnorm,mean(sqrt(datos_FULL$flujo)),sd(sqrt(datos_FULL$flujo)))
hist(sqrt(datos_FULL$flujo))


flujo_Sqrt=sqrt(datos_FULL$flujo)
edad<-datos_FULL$edad
mes<-datos_FULL$mes
estacion=datos_FULL$ESTACION

#Hacer base con flujo transformado y variables predictoras
base_anovas=data.frame(flujo_Sqrt,edad,mes,estacion)
levels(base_anovas$estacion)
View(base_anovas)
names(base_anovas)

anova_dos_factores <- aov(flujo_Sqrt ~ edad + mes + edad*mes,
                           data =base_anovas)
summary(anova_dos_factores)

#simple
lm_dos_factores <- lm(flujo_Sqrt ~ edad + mes + edad*mes,
                          data =base_anovas)
summary(lm_dos_factores)
?aov
aov(lm_dos_factores)
anova(lm_dos_factores)
plot(lm_dos_factores)

#Anova
anova_tres_factores <- aov(flujo_Sqrt ~ edad + estacion,
                           data =base_anovas)
summary(anova_tres_factores)



