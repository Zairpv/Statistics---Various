library(agricolae)
dataPERP = read.table("clipboard",header=T)
dataPERP

#######################################################################

# t de student:

#sincronizamos carpeta
setwd("C:/Users/Alumno/Documents/Alex_19°_E/Datos Virgilio_2")

#Datos
PERPESO1 <-read.csv("PERPESO1.csv",header = TRUE)
PERPESO1


library(moments)
library(stats)

attach(PERPESO1)
summary(PERPESO1)

#prueba de normalidad
shapiro.test(Predio.A) 
shapiro.test(Predio.B) 

#Toma de decisión:
#Sig(p valor) > alfa: No rechazar H0 (normal).
#Sig(p valor) < alfa: Rechazar H0 (no normal)

#Donde alfa representa la significancia, que en este ejemplo hipotético es igual al 5% (0,05).


#graficos para verificar tu normalidad
library(car)
library(carData)
qqPlot(Predio.A)
qqPlot(Predio.B)


#t student
t.test(Predio.A,Predio.B)


#####################################################

#ANOVA GLM

DAPERPESO=read.table("clipboard", header=T)
attach(DAPERPESO)
DAPERPESO
anaDAPERPESO = glm(PERPESOL ~Trat1L , family="poisson", data=DAPERPESO)
anova(anaDAPERPESO, test="Chisq")

summary(anaDAPERPESO)


ADECAduncan1 <-duncan.test(anaDAPERPESO,"data$",alpha=0.05)
ADECAduncan1

ADECAtukey<-HSD.test(anaDAPERPESO,"data$Trat1L",alpha=0.05)
ADECAtukey


bar.group(ADECAtukey$group, ylim=c(0,60), density=4,col="blue")

bar.group(ADECAduncan$group, ylim=c(0,60), density=4,col="blue")









##Comparación de efectos de tratamientos bajo Modelo ajustado

install.packages("contrast")
install.packages("lsmeans")
install.packages("Rtool")


Comp1 = lsmeans(anaDAPERPESO, "Trat1L")
Contrasts = list(B_vs_C = c(0,  0, 1, -1))
Cconv_tabs <- contrast(Comp1,Contrasts, adjust="sidak")


