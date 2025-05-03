#-------------------------------------------------------------
#Codigo K PREPARAR CONSOLA
#-------------------------------------------------------------

rm(list = ls())
datos_FULL=read.table("clipboard",header=T,sep="\t")
View(datos_FULL)
str(datos_FULL)
options(max.print=1000000)
setwd("D:/zaira/Documents/Karen")
windowsFonts(A = windowsFont("Times New Roman"))


#-------------------------------------------------------------
#1. Hacer vectores
#-------------------------------------------------------------

#Paso 1. Definir vectores
datos_FULL$edad=as.factor(datos_FULL$edad) #F EDAD
levels(datos_FULL$edad)

datos_FULL$mes=as.factor(datos_FULL$mes) #F MES
levels(datos_FULL$mes)

datos_FULL$LABEL_MES=as.factor(datos_FULL$mes) # Mes Etiqueta 
levels(datos_FULL$mes)

datos_FULL$conglomerado=as.factor(datos_FULL$conglomerado)
levels(datos_FULL$conglomerado)


#-------------------------------------------------------------
#SEPARAR LAS BASES POR EDADES
#-------------------------------------------------------------
levels(datos_FULL$edad)
names(datos_FULL)

(Datos_Edad10=datos_FULL[datos_FULL$edad=="10",])
View(Datos_Edad10)

(Datos_Edad20=datos_FULL[datos_FULL$edad=="20",])
View(Datos_Edad20)

(Datos_Edad30=datos_FULL[datos_FULL$edad=="30",])
View(Datos_Edad30)

(Datos_Edad33=datos_FULL[datos_FULL$edad=="33",])
View(Datos_Edad33)

(Datos_Edad85=datos_FULL[datos_FULL$edad=="85",])
View(Datos_Edad85)

#-------------------------------------------------------------
#SEPARACION DE TABLAS POR EDAD -- MEDIA Y DESVIACION ESTANDAR
#-------------------------------------------------------------
vectorEdad10=c("10","10","10","10","10","10","10","10","10","10","10","10")
vectorEdad20=c("20","20","20","20","20","20","20","20","20","20","20","20")
vectorEdad30=c("30","30","30","30","30","30","30","30","30","30","30","30")
vectorEdad33=c("33","33","33","33","33","33","33","33","33","33","33","33")
vectorEdad85=c("85","85","85","85","85","85","85","85","85","85","85","85")


View(vectorEdad10)

#Media y sd L 2013 por edad
library(dplyr)

#edad 10
names(datos_FULL)

mean_sd_FLUJO10=Datos_Edad10 %>%
  group_by(LABEL_MES) %>%
  summarise_at(vars(flujo),
               list(mean=mean,
                    sd=sd)) %>%
  as.data.frame()

View(mean_sd_FLUJO10) #Solo incluye la media y la sd de edad 10 de flujo
mean_sd_FLUJO10=data.frame(mean_sd_FLUJO10,vectorEdad10) #Pegar etiqueta edad
names(mean_sd_FLUJO10)
names(mean_sd_FLUJO10)=c("Mes","Flujo_mean","Flujo_SD","Edad")
View(mean_sd_FLUJO10)


#edad 20
names(datos_FULL)

mean_sd_FLUJO20=Datos_Edad20 %>%
  group_by(LABEL_MES) %>%
  summarise_at(vars(flujo),
               list(mean=mean,
                    sd=sd)) %>%
  as.data.frame()

View(mean_sd_FLUJO20) #Solo incluye la media y la sd de edad 20 de flujo
mean_sd_FLUJO20=data.frame(mean_sd_FLUJO20,vectorEdad20) #Pegar etiqueta edad
names(mean_sd_FLUJO20)
names(mean_sd_FLUJO20)=c("Mes","Flujo_mean","Flujo_SD","Edad")
View(mean_sd_FLUJO20)


#edad 30
names(datos_FULL)

mean_sd_FLUJO30=Datos_Edad30 %>%
  group_by(LABEL_MES) %>%
  summarise_at(vars(flujo),
               list(mean=mean,
                    sd=sd)) %>%
  as.data.frame()

View(mean_sd_FLUJO30) #Solo incluye la media y la sd de edad 30 de flujo
mean_sd_FLUJO30=data.frame(mean_sd_FLUJO30,vectorEdad30) #Pegar etiqueta edad
names(mean_sd_FLUJO30)
names(mean_sd_FLUJO30)=c("Mes","Flujo_mean","Flujo_SD","Edad")
View(mean_sd_FLUJO30)


#edad 33
names(datos_FULL)

mean_sd_FLUJO33=Datos_Edad33 %>%
  group_by(LABEL_MES) %>%
  summarise_at(vars(flujo),
               list(mean=mean,
                    sd=sd)) %>%
  as.data.frame()

View(mean_sd_FLUJO33) #Solo incluye la media y la sd de edad 33 de flujo
mean_sd_FLUJO33=data.frame(mean_sd_FLUJO33,vectorEdad33) #Pegar etiqueta edad
names(mean_sd_FLUJO33)
names(mean_sd_FLUJO33)=c("Mes","Flujo_mean","Flujo_SD","Edad")
View(mean_sd_FLUJO33)


#edad 85
names(datos_FULL)

mean_sd_FLUJO85=Datos_Edad85 %>%
  group_by(LABEL_MES) %>%
  summarise_at(vars(flujo),
               list(mean=mean,
                    sd=sd)) %>%
  as.data.frame()

View(mean_sd_FLUJO85) #Solo incluye la media y la sd de edad 85 de flujo
mean_sd_FLUJO85=data.frame(mean_sd_FLUJO85,vectorEdad85) #Pegar etiqueta edad
names(mean_sd_FLUJO85)
names(mean_sd_FLUJO85)=c("Mes","Flujo_mean","Flujo_SD","Edad")
View(mean_sd_FLUJO85)


##### Unir todas las edades 
media_sd_flux_edad=rbind(mean_sd_FLUJO10,mean_sd_FLUJO20,mean_sd_FLUJO30,mean_sd_FLUJO33,mean_sd_FLUJO85)
View(media_sd_flux_edad)

#Guardar en excel base 
#write.csv(media_sd_flux_edad,"media_sd_flux_edad.csv",row.names = TRUE)

#----------------------------------------------------------------------
##### Grafica de variacion de flujo por edad 
#----------------------------------------------------------------------------
library(ggplot2)
library(ggpubr)

#Mgha_expression=expression("Mg ha"^"-1")
co2_expression=expression("Flujos de CO"[2])

base_mesesordenados=read.table("clipboard",header=T,sep="\t")
View(base_mesesordenados)

base_mesesordenados$Edad=as.factor(base_mesesordenados$Edad)
base_mesesordenados$Mes=as.factor(base_mesesordenados$Mes)
base_mesesordenados$ORDEN.MES=as.factor(base_mesesordenados$ORDEN.MES)

#flujo_edad_PLOT

#jpeg(filename = "Fig 2.jpeg",width = 190,height = 100,units = "mm",res = 300) #No cambiar 190 
Fig2=ggline(base_mesesordenados, x = "ORDEN.MES", y = "Flujo_mean", color = "Edad",
       add = c("Flujo_SD", "dotplot"),
       numeric.x.axis = FALSE,
       xlab = "Meses",
       ylab = co2_expression,
       palette = c("#daaf2a", "#7ea16b","#784b24","596f62","#1c3144"))
#dev.off()
Fig2

## Pendiente "Modificar labels"


#####-------------------------------------------------------------
#grafica general de flujo
#-----------------------------------------------------------------------------

Flujo_general=read.table("clipboard",header=T,sep="\t")
Flujo_general$MES=as.factor(Flujo_general$MES)
Flujo_general$MES.ORDEN=as.factor(Flujo_general$MES.ORDEN)

names(Flujo_general)

#jpeg(filename = "Fig 1.jpeg",width = 190,height = 100,units = "mm",res = 300) #No cambiar 190 
Fig1=ggline(Flujo_general, x = "MES.ORDEN", y = "FLUJO" ,
       numeric.x.axis = FALSE,
       xlab = "Meses",
       ylab = co2_expression)
#dev.off()
Fig1

jpeg(filename = "Fig 3.jpeg",width = 190,height =150,units = "mm",res = 300) #No cambiar 190 
ggarrange(Fig1,Fig2,ncol = 1,nrow = 2)
dev.off()


