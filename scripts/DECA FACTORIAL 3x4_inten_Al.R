#DECA CON ARREGLO FACTORIAL 3X4
library(agricolae)

dataV70=read.table("clipboard",header=T)
View(dataV70)

attach(dataV70)

ESTADO=as.factor(ESTADO) 
FECHA=as.factor(FECHA)

V70DECA=aov(PERPESO~ESTADO*FECHA)
summary(V70DECA)

#O ESTE OTRO:
modeloV<-lm(PERPESO~(ESTADO+FECHA)^2)
V70DECA2<-aov(modeloV)
summary(V70DECA2)
summary(V70DECA) #Hay significancia. Se pueden hacer graficas de interacciones

aov(V70DECA)
?aov

#¿Ambos modelos dan la misma salida, pero uno es aditivo y otro es de efectos?
#¿Explicación biológica?
coef(V70DECA) #Estos permiten

shapiro.test(V70DECA$residuals) #Son mas de 50 registros, se necesita el de Kolmogorov
library(nortest)
lillie.test(V70DECA$residuals) #Con este si se cumple la normalidad

plot(V70DECA$fitted.values, V70DECA$residuals, main="Residuales vs Predichos", pch=20,
     ylab = "residuals",xlab = "fitted values")



#Tukey para cada factor y combinacion

ObTukey=HSD.test(V70DECA,"ESTADO")
ObTukey


OTukey=HSD.test(V70DECA,"FECHA")
OTukey


bOTukey=HSD.test(V70DECA,"PERPESO")
bOTukey

### Gráficas complementarias
library(phia)
Grafica <- interactionMeans(V70DECA)
plot(Grafica)
par(mfrow=c(1,1))
dev.off()


##Verificación del dca
lillie.test(rstandard(V70DECA)) #Son normales
plot(rstandard(V70DECA),main = "gráfica de residuos estandarizados") #Son normales
qqnorm(rstandard(V70DECA))
qqline(rstandard(V70DECA))
#hay homogeneidad de varianzas y normalidad --> modelo aprobado

#Ver los valores ajustados por la regresion
fitted(V70DECA)
dev.off()
plot(fitted(V70DECA),PERPESO,col=c("red","blue"),
     pch=20,main = "gráfica de valores predichos vs observados",
     ylab = "valores observados",xlab = "valores predichos")
legend(50,50,col = c("red","blue"),legend = c("predichos","observados"),pch = 20)

#Estas funciones son para obtener de forma individual las gráficas anteriores
#grafica de efectos principales
efectos=data.frame(ESTADO,FECHA,PERPESO)
plot.design(efectos,fun = "mean",main = "Gráfica de efectos principales",ylab = "Pérdida de peso",xlab = "Factores")
#Interpretación: el estado 3 da la mayor pérdida de peso y con la fecha 1. 
#es decir, que dadas las condiciones de fecha, el estado 2 es el que provoca la
#menor pérdida de peso promedio



#gráfica de interaccion
interaction.plot(ESTADO,FECHA,PERPESO, main="interacción estado*fecha",col = c(1:4))
#El estado 2 provoca la menor pérdida de peso en todas las condiciones de fecha


#Test Duncan
duncan.test(V70DECA,"FECHA",group = FALSE)$comparison
duncan.test(V70DECA,"ESTADO",group = FALSE)$comparison


