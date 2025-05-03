###1. Consola
rm(list=ls()) 
options(max.print = 999000000) 
setwd("D:/zaira/Documents/08. R scripts/Diseño_tratamientos")

### 1. Leer datos. 
datos<- read.csv("Tabla para R.csv",header = TRUE)
View(datos)

### 2. Definir las variables factor (Es importante hacerlo desde el principio para que
#los dataframes que sean creados tengas las mismas propiedades)
names(datos)

datos$ORGANOS=as.factor(datos$ORGANOS)
levels(datos$ORGANOS)

datos$TRATAMIENTO=as.factor(datos$TRATAMIENTO)
levels(datos$TRATAMIENTO)
levels(datos$TRATAMIENTO)=c("125 mg/mL","250 mg/mL","8 mg/mL","Control","Vehículo")

datos$TIEMPO=as.factor(datos$TIEMPO)
levels(datos$TIEMPO)

datos$REPETICIONES=as.factor(datos$REPETICIONES)
levels(datos$REPETICIONES)

### 3. Cargar librerias
library(agricolae)

#Antes de evaluar las diferencias entre medias necesitas conocer tus datos
#para ello hay que hacer un analisis descriptivo y esto requiere que vayamos 
#dividiendo tus datos hasta el nivel más bajo  

#------------------------------------------------------
# 1. PARTICION DEL DATA FRAME EN FUNCIÓN DE LOS ORGANOS
#------------------------------------------------------
names(datos) 
#Primero partimos los datos por organos y creamos tres dataframes

#Datos Higado
datos_higado=datos[datos$ORGANOS=="Higado",]
View(datos_higado)
#Datos suero
datos_suero=datos[datos$ORGANOS=="Suero",]
View(datos_suero)
#Datos orina
datos_orina=datos[datos$ORGANOS=="Orina",]
View(datos_orina)

#------------------------------------------------------
# 2. ANALISIS DESCRIPTIVO
#------------------------------------------------------
library(psych)

#En este caso debes ir capturando manualmente tus datos descriptivos 
#Te apoye con la extracción de las tablas en un bloc de notas, pero lo fui ordenando manualmente ya que R ordena los tiempos de forma diferente al orden real
#Utilice una funcion que te agrupa por tratamiento y a su vez por cada tiempo, entonces toma en consideración los 4 valores de las repeticiones para sacar
#cada uno de los estadisticos

#vars: es el numero de la columna, no tiene importancia estadistica
#n: lo ideal es que siempre sea 4, porque es el numero de repeticiones con valores
#mean: promedio o media
#sd: desviación estandar
#mad: La media recortada (el valor predeterminado recorta el 10 % de las observaciones de cada extremo) (No es usualmente incorporado en un analisis descriptivo)
#mad: The median absolute deviation (from the median)
#min: The minimum value
#max: The maximum value
#range: The range of values (max – min)
#skew: The skewness
#kurtosis: The kurtosis
#se: The standard error

#Nota. Te recomiendo solo reportar : n, min, max, mean, sd, skew, kurtosis y se (en ese orden)

#Resumen para higado
names(datos_higado)
describeBy(datos_higado[5:21],list(datos_higado$TIEMPO,datos_higado$TRATAMIENTO))
describeBy(datos_orina[5:21],list(datos_orina$TIEMPO,datos_orina$TRATAMIENTO))
describeBy(datos_suero[5:21],list(datos_suero$TIEMPO,datos_suero$TRATAMIENTO))



#------------------------------------------------------
# 3. Boxplots
#------------------------------------------------------
library(ggplot2)
View(datos)
names(datos)

#Nota. Debido a la falta de datos los boxplots salen de esta forma, se iran ampliando conforme integres la información faltante
#En este enlace puedes reemplazar los codigos de color que te agraden. Solo cambia esos codigos que estan remarcados con el codigo de color de la siguiente pag
#https://colorhunt.co/  

#Las imagenes apareceran en tu carpeta si activas las lineas que tienen #. Si le vuelves a anteponer #, volveran a salir en la consola
#Las imagenes ya tienen la medida estandar para publicación en artículos. solo no modifiques el valor width = 190
#jpeg(filename = "Boxplot_Rutina.jpeg",width = 190,height = 130,units = "mm",res = 300) #No cambiar valor valor 190 
ggplot(datos, aes(x=TRATAMIENTO, y=RUTINA,fill=ORGANOS,)) + 
  geom_boxplot()+
  scale_fill_manual(values=c("#7C96AB","#FFD3B0","#F7D060")) +
  scale_x_discrete(limits=c("Control", "Vehículo", "8 mg/mL","125 mg/mL","250 mg/mL")) +
  labs(title="Metabolito: Rutina",x="Tratamientos", y = "mg/g.p.s de peso de extracto seco",fill="Órgano")+
  facet_wrap(~TIEMPO,ncol = 2,nrow = 2)+
  facet_grid(factor(TIEMPO, levels=c("T1","T6", "T24","T48"))~.)
#dev.off()
      
#jpeg(filename = "Boxplot_Naringenina.jpeg",width = 190,height = 130,units = "mm",res = 300) #No cambiar 190 
ggplot(datos, aes(x=TRATAMIENTO, y=NARINGENINA,fill=ORGANOS,)) + 
  geom_boxplot()+
  scale_fill_manual(values=c("#7C96AB","#FFD3B0","#F7D060")) +
  scale_x_discrete(limits=c("Control", "Vehículo", "8 mg/mL","125 mg/mL","250 mg/mL")) +
  labs(title="Metabolito: Naringenina",x="Tratamientos", y = "mg/g.p.s de peso de extracto seco",fill="Órgano")+
  facet_wrap(~TIEMPO,ncol = 2,nrow = 2)+
  facet_grid(factor(TIEMPO, levels=c("T1","T6", "T24","T48"))~.)
#dev.off()

#jpeg(filename = "Boxplot_Floretina.jpeg",width = 190,height = 130,units = "mm",res = 300) #No cambiar 190 
ggplot(datos, aes(x=TRATAMIENTO, y=FLORETINA,fill=ORGANOS,)) + 
  geom_boxplot()+
  scale_fill_manual(values=c("#7C96AB","#FFD3B0","#F7D060")) +
  scale_x_discrete(limits=c("Control", "Vehículo", "8 mg/mL","125 mg/mL","250 mg/mL")) +
  labs(title="Metabolito: Floretina",x="Tratamientos", y = "mg/g.p.s de peso de extracto seco",fill="Órgano")+
  facet_wrap(~TIEMPO,ncol = 2,nrow = 2)+
  facet_grid(factor(TIEMPO, levels=c("T1","T6", "T24","T48"))~.)
#dev.off()

#jpeg(filename = "Boxplot_Morina.jpeg",width = 190,height = 130,units = "mm",res = 300) #No cambiar 190 
ggplot(datos, aes(x=TRATAMIENTO, y=MORINA,fill=ORGANOS,)) + 
  geom_boxplot()+
  scale_fill_manual(values=c("#7C96AB","#FFD3B0","#F7D060")) +
  scale_x_discrete(limits=c("Control", "Vehículo", "8 mg/mL","125 mg/mL","250 mg/mL")) +
  labs(title="Metabolito: Morina",x="Tratamientos", y = "mg/g.p.s de peso de extracto seco",fill="Órgano")+
  facet_wrap(~TIEMPO,ncol = 2,nrow = 2)+
  facet_grid(factor(TIEMPO, levels=c("T1","T6", "T24","T48"))~.)
#dev.off()

#jpeg(filename = "Boxplot_Apigenina.jpeg",width = 190,height = 130,units = "mm",res = 300) #No cambiar 190 
ggplot(datos, aes(x=TRATAMIENTO, y=APIGENINA,fill=ORGANOS,)) + 
  geom_boxplot()+
  scale_fill_manual(values=c("#7C96AB","#FFD3B0","#F7D060")) +
  scale_x_discrete(limits=c("Control", "Vehículo", "8 mg/mL","125 mg/mL","250 mg/mL")) +
  labs(title="Metabolito: Apigenina",x="Tratamientos", y = "mg/g.p.s de peso de extracto seco",fill="Órgano")+
  facet_wrap(~TIEMPO,ncol = 2,nrow = 2)+
  facet_grid(factor(TIEMPO, levels=c("T1","T6", "T24","T48"))~.)
#dev.off()

#jpeg(filename = "Boxplot_Miricetina.jpeg",width = 190,height = 130,units = "mm",res = 300) #No cambiar 190 
ggplot(datos, aes(x=TRATAMIENTO, y=MIRICETINA,fill=ORGANOS,)) + 
  geom_boxplot()+
  scale_fill_manual(values=c("#7C96AB","#FFD3B0","#F7D060")) +
  scale_x_discrete(limits=c("Control", "Vehículo", "8 mg/mL","125 mg/mL","250 mg/mL")) +
  labs(title="Metabolito: Miricetina",x="Tratamientos", y = "mg/g.p.s de peso de extracto seco",fill="Órgano")+
  facet_wrap(~TIEMPO,ncol = 2,nrow = 2)+
  facet_grid(factor(TIEMPO, levels=c("T1","T6", "T24","T48"))~.)
#dev.off()

#jpeg(filename = "Boxplot_Isorhamnetina.jpeg",width = 190,height = 130,units = "mm",res = 300) #No cambiar 190 
ggplot(datos, aes(x=TRATAMIENTO, y=ISORHAMNETINA,fill=ORGANOS,)) + 
  geom_boxplot()+
  scale_fill_manual(values=c("#7C96AB","#FFD3B0","#F7D060")) +
  scale_x_discrete(limits=c("Control", "Vehículo", "8 mg/mL","125 mg/mL","250 mg/mL")) +
  labs(title="Metabolito: Isorhamnetina",x="Tratamientos", y = "mg/g.p.s de peso de extracto seco",fill="Órgano")+
  facet_wrap(~TIEMPO,ncol = 2,nrow = 2)+
  facet_grid(factor(TIEMPO, levels=c("T1","T6", "T24","T48"))~.)
#dev.off()

#jpeg(filename = "Boxplot_Hesperidina.jpeg",width = 190,height = 130,units = "mm",res = 300) #No cambiar 190 
ggplot(datos, aes(x=TRATAMIENTO, y=HESPERIDINA,fill=ORGANOS,)) + 
  geom_boxplot()+
  scale_fill_manual(values=c("#7C96AB","#FFD3B0","#F7D060")) +
  scale_x_discrete(limits=c("Control", "Vehículo", "8 mg/mL","125 mg/mL","250 mg/mL")) +
  labs(title="Metabolito: Hesperidina",x="Tratamientos", y = "mg/g.p.s de peso de extracto seco",fill="Órgano")+
  facet_wrap(~TIEMPO,ncol = 2,nrow = 2)+
  facet_grid(factor(TIEMPO, levels=c("T1","T6", "T24","T48"))~.)
#dev.off()

#jpeg(filename = "Boxplot_Kaemferol.jpeg",width = 190,height = 130,units = "mm",res = 300) #No cambiar 190 
ggplot(datos, aes(x=TRATAMIENTO, y=KAEMFEROL,fill=ORGANOS,)) + 
  geom_boxplot()+
  scale_fill_manual(values=c("#7C96AB","#FFD3B0","#F7D060")) +
  scale_x_discrete(limits=c("Control", "Vehículo", "8 mg/mL","125 mg/mL","250 mg/mL")) +
  labs(title="Metabolito: Kaemferol",x="Tratamientos", y = "mg/g.p.s de peso de extracto seco",fill="Órgano")+
  facet_wrap(~TIEMPO,ncol = 2,nrow = 2)+
  facet_grid(factor(TIEMPO, levels=c("T1","T6", "T24","T48"))~.)
#dev.off()

#jpeg(filename = "Boxplot_Quercetina.jpeg",width = 190,height = 130,units = "mm",res = 300) #No cambiar 190 
ggplot(datos, aes(x=TRATAMIENTO, y=QUERCETINA,fill=ORGANOS,)) + 
  geom_boxplot()+
  scale_fill_manual(values=c("#7C96AB","#FFD3B0","#F7D060")) +
  scale_x_discrete(limits=c("Control", "Vehículo", "8 mg/mL","125 mg/mL","250 mg/mL")) +
  labs(title="Metabolito: Quercetina",x="Tratamientos", y = "mg/g.p.s de peso de extracto seco",fill="Órgano")+
  facet_wrap(~TIEMPO,ncol = 2,nrow = 2)+
  facet_grid(factor(TIEMPO, levels=c("T1","T6", "T24","T48"))~.)
#dev.off()

#jpeg(filename = "Boxplot_Catequina.jpeg",width = 190,height = 130,units = "mm",res = 300) #No cambiar 190 
ggplot(datos, aes(x=TRATAMIENTO, y=CATEQUINA,fill=ORGANOS,)) + 
  geom_boxplot()+
  scale_fill_manual(values=c("#7C96AB","#FFD3B0","#F7D060")) +
  scale_x_discrete(limits=c("Control", "Vehículo", "8 mg/mL","125 mg/mL","250 mg/mL")) +
  labs(title="Metabolito: Catequina",x="Tratamientos", y = "mg/g.p.s de peso de extracto seco",fill="Órgano")+
  facet_wrap(~TIEMPO,ncol = 2,nrow = 2)+
  facet_grid(factor(TIEMPO, levels=c("T1","T6", "T24","T48"))~.)
#dev.off()

#jpeg(filename = "Boxplot_Floritzina.jpeg",width = 190,height = 130,units = "mm",res = 300) #No cambiar 190 
ggplot(datos, aes(x=TRATAMIENTO, y=FLORITZINA,fill=ORGANOS,)) + 
  geom_boxplot()+
  scale_fill_manual(values=c("#7C96AB","#FFD3B0","#F7D060")) +
  scale_x_discrete(limits=c("Control", "Vehículo", "8 mg/mL","125 mg/mL","250 mg/mL")) +
  labs(title="Metabolito: Floritzina",x="Tratamientos", y = "mg/g.p.s de peso de extracto seco",fill="Órgano")+
  facet_wrap(~TIEMPO,ncol = 2,nrow = 2)+
  facet_grid(factor(TIEMPO, levels=c("T1","T6", "T24","T48"))~.)
#dev.off()

#jpeg(filename = "Boxplot_Cucurbitacina D.jpeg",width = 190,height = 130,units = "mm",res = 300) #No cambiar 190 
ggplot(datos, aes(x=TRATAMIENTO, y=CUCURBITACINA.D,fill=ORGANOS,)) + 
  geom_boxplot()+
  scale_fill_manual(values=c("#7C96AB","#FFD3B0","#F7D060")) +
  scale_x_discrete(limits=c("Control", "Vehículo", "8 mg/mL","125 mg/mL","250 mg/mL")) +
  labs(title="Metabolito: Cucurbitacina D",x="Tratamientos", y = "mg/g.p.s de peso de extracto seco",fill="Órgano")+
  facet_wrap(~TIEMPO,ncol = 2,nrow = 2)+
  facet_grid(factor(TIEMPO, levels=c("T1","T6", "T24","T48"))~.)
#dev.off()

#jpeg(filename = "Boxplot_Cucurbitacina I.jpeg",width = 190,height = 130,units = "mm",res = 300) #No cambiar 190 
ggplot(datos, aes(x=TRATAMIENTO, y=CUCURBITACINA.I,fill=ORGANOS,)) + 
  geom_boxplot()+
  scale_fill_manual(values=c("#7C96AB","#FFD3B0","#F7D060")) +
  scale_x_discrete(limits=c("Control", "Vehículo", "8 mg/mL","125 mg/mL","250 mg/mL")) +
  labs(title="Metabolito: Cucurbitacina I",x="Tratamientos", y = "mg/g.p.s de peso de extracto seco",fill="Órgano")+
  facet_wrap(~TIEMPO,ncol = 2,nrow = 2)+
  facet_grid(factor(TIEMPO, levels=c("T1","T6", "T24","T48"))~.)
#dev.off()

#jpeg(filename = "Boxplot_Cucurbitacina IIA.jpeg",width = 190,height = 130,units = "mm",res = 300) #No cambiar 190 
ggplot(datos, aes(x=TRATAMIENTO, y=CUCURBITACINA.IIA,fill=ORGANOS,)) + 
  geom_boxplot()+
  scale_fill_manual(values=c("#7C96AB","#FFD3B0","#F7D060")) +
  scale_x_discrete(limits=c("Control", "Vehículo", "8 mg/mL","125 mg/mL","250 mg/mL")) +
  labs(title="Metabolito: Cucurbitacina IIA",x="Tratamientos", y = "mg/g.p.s de peso de extracto seco",fill="Órgano")+
  facet_wrap(~TIEMPO,ncol = 2,nrow = 2)+
  facet_grid(factor(TIEMPO, levels=c("T1","T6", "T24","T48"))~.)
#dev.off()

#jpeg(filename = "Boxplot_Cucurbitacina B.jpeg",width = 190,height = 130,units = "mm",res = 300) #No cambiar 190 
ggplot(datos, aes(x=TRATAMIENTO, y=CUCURBITACINA.B,fill=ORGANOS,)) + 
  geom_boxplot()+
  scale_fill_manual(values=c("#7C96AB","#FFD3B0","#F7D060")) +
  scale_x_discrete(limits=c("Control", "Vehículo", "8 mg/mL","125 mg/mL","250 mg/mL")) +
  labs(title="Metabolito: Cucurbitacina B",x="Tratamientos", y = "mg/g.p.s de peso de extracto seco",fill="Órgano")+
  facet_wrap(~TIEMPO,ncol = 2,nrow = 2)+
  facet_grid(factor(TIEMPO, levels=c("T1","T6", "T24","T48"))~.)
#dev.off()

#jpeg(filename = "Boxplot_Cucurbitacina E.jpeg",width = 190,height = 130,units = "mm",res = 300) #No cambiar 190 
ggplot(datos, aes(x=TRATAMIENTO, y=CUCURBITACINA.E,fill=ORGANOS,)) + 
  geom_boxplot()+
  scale_fill_manual(values=c("#7C96AB","#FFD3B0","#F7D060")) +
  scale_x_discrete(limits=c("Control", "Vehículo", "8 mg/mL","125 mg/mL","250 mg/mL")) +
  labs(title="Metabolito: Cucurbitacina E",x="Tratamientos", y = "mg/g.p.s de peso de extracto seco",fill="Órgano")+
  facet_wrap(~TIEMPO,ncol = 2,nrow = 2)+
  facet_grid(factor(TIEMPO, levels=c("T1","T6", "T24","T48"))~.)
#dev.off()

#Nota: Hay algunos boxplots que solo tienen dos órganos, no t preocupes, una vez que llenes tu excel, iran apareciendo en los diágramas


#------------------------------------------------------
# 4. Normalidad general
#------------------------------------------------------
library(nortest)
names(datos)
#La prueba de normalidad debe hacerse para cada uno de los metabolitos que evalues
length(datos$RUTINA) #Como tienes más de 50 datos, no t recomiendo utilizar la prueba de Shapiro para normalidad
#En su lugar, la prueba mas adecuada es la de Anderson-Darling 
#Debido a la información incompleta es probable que la normalidad no se alcance
#Una vez que tengas todas tus mediciones vuelve a correr los datos
#Si para entonces los datos no alcanzan normalidad hay q transformar cada variable de metabolitos

#Prueba de hipotesis
#Ho: los datos son normales
#Ha: lo contrario
#Rechazar Ho si pvalue < 0.05. Lo que significa que los datos no son normales

ad.test(datos$RUTINA) #Se rechaza Ho
ad.test(datos$NARINGENINA)  #Se rechaza Ho
ad.test(datos$FLORETINA)  #Se rechaza Ho
ad.test(datos$MORINA)  #Se rechaza Ho
ad.test(datos$APIGENINA)  #Se rechaza Ho
ad.test(datos$MIRICETINA)  #Se rechaza Ho
ad.test(datos$ISORHAMNETINA)  #Se rechaza Ho
ad.test(datos$HESPERIDINA)  #Se rechaza Ho
ad.test(datos$KAEMFEROL)  #Se rechaza Ho
ad.test(datos$QUERCETINA)  #Se rechaza Ho
ad.test(datos$CATEQUINA)  #Se rechaza Ho
ad.test(datos$FLORITZINA)  #Se rechaza Ho
ad.test(datos$CUCURBITACINA.D) #Se rechaza Ho
ad.test(datos$CUCURBITACINA.I) #Se rechaza Ho
ad.test(datos$CUCURBITACINA.IIA) #Se rechaza Ho
ad.test(datos$CUCURBITACINA.B) #Se rechaza Ho
ad.test(datos$CUCURBITACINA.E) #Se rechaza Ho

#Por si te llegaran a pedir tus asesores la de Shapiro aqui va el codigo
#Luego esa es la que piden por ser la mas conocida, pero en tus datos no es la optima porque SHAPIRo se utiliza para muestras pequeñas (n menor a 50 y tenemos 240)
#La prueba de decisión es la misma que en la anterior

shapiro.test(datos$RUTINA)  #Se rechaza Ho
shapiro.test(datos$NARINGENINA) #Se rechaza Ho
shapiro.test(datos$FLORETINA) #Se rechaza Ho
shapiro.test(datos$MORINA) #Se rechaza Ho
shapiro.test(datos$APIGENINA) #Se rechaza Ho
shapiro.test(datos$MIRICETINA) #Se rechaza Ho
shapiro.test(datos$ISORHAMNETINA) #Se rechaza Ho
shapiro.test(datos$HESPERIDINA) #Se rechaza Ho
shapiro.test(datos$KAEMFEROL) #Se rechaza Ho
shapiro.test(datos$QUERCETINA) #Se rechaza Ho
shapiro.test(datos$CATEQUINA) #Se rechaza Ho
shapiro.test(datos$FLORITZINA) #Se rechaza Ho
shapiro.test(datos$CUCURBITACINA.D) #Se rechaza Ho
shapiro.test(datos$CUCURBITACINA.I) #Se rechaza Ho
shapiro.test(datos$CUCURBITACINA.IIA) #Se rechaza Ho
shapiro.test(datos$CUCURBITACINA.B) #Se rechaza Ho
shapiro.test(datos$CUCURBITACINA.E) #Se rechaza Ho
#Tampoco con esta prueba se alcanza la normalidad


#------------------------------------------------------
# 4. Normalidad por tiempos de cada órgano y metabolito
#------------------------------------------------------
levels(datos_higado$TIEMPO)
#Hay algunas pruebas que no pueden determinarse por la falta de datos
names(datos)


#METABOLITO: RUTINA
#Normalidad para higado
#Te dejo el codigo, una vez que se completen los datos debería de correr cualquiera de las dos pruebas. De momento no es posible
ad.test( datos_higado$RUTINA[datos_higado$TIEMPO == "T1"] ) 
ad.test( datos_higado$RUTINA[datos_higado$TIEMPO == "T6"] )
ad.test( datos_higado$RUTINA[datos_higado$TIEMPO == "T24"] )
ad.test( datos_higado$RUTINA[datos_higado$TIEMPO == "T48"] )
shapiro.test( datos_higado$RUTINA[datos_higado$TIEMPO == "T1"] )  #No es normal aun
shapiro.test( datos_higado$RUTINA[datos_higado$TIEMPO == "T6"] )
shapiro.test( datos_higado$RUTINA[datos_higado$TIEMPO == "T24"] )
shapiro.test( datos_higado$RUTINA[datos_higado$TIEMPO == "T48"] )

#Normalidad para orina
ad.test( datos_orina$RUTINA[datos_orina$TIEMPO == "T1"] ) #No es normal aun
ad.test( datos_orina$RUTINA[datos_orina$TIEMPO == "T6"] ) #No es normal aun
ad.test( datos_orina$RUTINA[datos_orina$TIEMPO == "T24"] ) #No es normal aun
ad.test( datos_orina$RUTINA[datos_orina$TIEMPO == "T48"] ) #No es normal aun

shapiro.test( datos_orina$RUTINA[datos_orina$TIEMPO == "T1"] ) #No es normal aun
shapiro.test( datos_orina$RUTINA[datos_orina$TIEMPO == "T6"] )#No es normal aun
shapiro.test( datos_orina$RUTINA[datos_orina$TIEMPO == "T24"] ) #SI es normal 
shapiro.test( datos_orina$RUTINA[datos_orina$TIEMPO == "T48"] )#No es normal aun

#Normalidad para suero
ad.test( datos_suero$RUTINA[datos_suero$TIEMPO == "T1"] ) #Si es normal 
ad.test( datos_suero$RUTINA[datos_suero$TIEMPO == "T6"] ) #No es normal aun
ad.test( datos_suero$RUTINA[datos_suero$TIEMPO == "T24"] )#No es normal aun
ad.test( datos_suero$RUTINA[datos_suero$TIEMPO == "T48"] )#No es normal aun

shapiro.test( datos_suero$RUTINA[datos_suero$TIEMPO == "T1"] ) #Si es normal 
shapiro.test( datos_suero$RUTINA[datos_suero$TIEMPO == "T6"] ) #No es normal aun
shapiro.test( datos_suero$RUTINA[datos_suero$TIEMPO == "T24"] ) #No es normal aun
shapiro.test( datos_suero$RUTINA[datos_suero$TIEMPO == "T48"] )#No es normal aun



#METABOLITO: NARINGENINA
#Normalidad para higado
#Te dejo el codigo, una vez que se completen los datos debería de correr cualquiera de las dos pruebas. De momento no es posible
ad.test( datos_higado$NARINGENINA[datos_higado$TIEMPO == "T1"] ) 
ad.test( datos_higado$NARINGENINA[datos_higado$TIEMPO == "T6"] )
ad.test( datos_higado$NARINGENINA[datos_higado$TIEMPO == "T24"] )
ad.test( datos_higado$NARINGENINA[datos_higado$TIEMPO == "T48"] )
shapiro.test( datos_higado$NARINGENINA[datos_higado$TIEMPO == "T1"] )  #No es normal aun
shapiro.test( datos_higado$NARINGENINA[datos_higado$TIEMPO == "T6"] )
shapiro.test( datos_higado$NARINGENINA[datos_higado$TIEMPO == "T24"] )
shapiro.test( datos_higado$NARINGENINA[datos_higado$TIEMPO == "T48"] )

#Normalidad para orina
ad.test( datos_orina$NARINGENINA[datos_orina$TIEMPO == "T1"] ) #No es normal aun
ad.test( datos_orina$NARINGENINA[datos_orina$TIEMPO == "T6"] ) #No es normal aun
ad.test( datos_orina$NARINGENINA[datos_orina$TIEMPO == "T24"] ) #No es normal aun
ad.test( datos_orina$NARINGENINA[datos_orina$TIEMPO == "T48"] ) #No es normal aun

shapiro.test( datos_orina$NARINGENINA[datos_orina$TIEMPO == "T1"] ) #No es normal aun
shapiro.test( datos_orina$NARINGENINA[datos_orina$TIEMPO == "T6"] )#No es normal aun
shapiro.test( datos_orina$NARINGENINA[datos_orina$TIEMPO == "T24"] ) #no es normal aun
shapiro.test( datos_orina$NARINGENINA[datos_orina$TIEMPO == "T48"] )#No es normal aun

#Normalidad para suero
ad.test( datos_suero$NARINGENINA[datos_suero$TIEMPO == "T1"] ) #no es normal aun
ad.test( datos_suero$NARINGENINA[datos_suero$TIEMPO == "T6"] ) #No es normal aun
ad.test( datos_suero$NARINGENINA[datos_suero$TIEMPO == "T24"] )#No es normal aun
ad.test( datos_suero$NARINGENINA[datos_suero$TIEMPO == "T48"] )#No es normal aun

shapiro.test( datos_suero$NARINGENINA[datos_suero$TIEMPO == "T1"] ) #no es normal aun
shapiro.test( datos_suero$NARINGENINA[datos_suero$TIEMPO == "T6"] ) #No es normal aun
shapiro.test( datos_suero$NARINGENINA[datos_suero$TIEMPO == "T24"] ) #No es normal aun
shapiro.test( datos_suero$NARINGENINA[datos_suero$TIEMPO == "T48"] )#No es normal aun


#METABOLITO: FLORETINA
#Normalidad para higado
#Te dejo el codigo, una vez que se completen los datos debería de correr cualquiera de las dos pruebas. De momento no es posible
ad.test( datos_higado$FLORETINA[datos_higado$TIEMPO == "T1"] ) #No es normal aun
ad.test( datos_higado$FLORETINA[datos_higado$TIEMPO == "T6"] )
ad.test( datos_higado$FLORETINA[datos_higado$TIEMPO == "T24"] )#No es normal aun
ad.test( datos_higado$FLORETINA[datos_higado$TIEMPO == "T48"] )
shapiro.test( datos_higado$FLORETINA[datos_higado$TIEMPO == "T1"] )  #No es normal aun
shapiro.test( datos_higado$FLORETINA[datos_higado$TIEMPO == "T6"] ) #Si es normal
shapiro.test( datos_higado$FLORETINA[datos_higado$TIEMPO == "T24"] )#No es normal aun
shapiro.test( datos_higado$FLORETINA[datos_higado$TIEMPO == "T48"] )

#Normalidad para orina
ad.test( datos_orina$FLORETINA[datos_orina$TIEMPO == "T1"] ) #No es normal aun
ad.test( datos_orina$FLORETINA[datos_orina$TIEMPO == "T6"] ) #No es normal aun
ad.test( datos_orina$FLORETINA[datos_orina$TIEMPO == "T24"] ) #No es normal aun
ad.test( datos_orina$FLORETINA[datos_orina$TIEMPO == "T48"] ) #No es normal aun

shapiro.test( datos_orina$FLORETINA[datos_orina$TIEMPO == "T1"] ) #No es normal aun
shapiro.test( datos_orina$FLORETINA[datos_orina$TIEMPO == "T6"] )#No es normal aun
shapiro.test( datos_orina$FLORETINA[datos_orina$TIEMPO == "T24"] ) #no es normal aun
shapiro.test( datos_orina$FLORETINA[datos_orina$TIEMPO == "T48"] )#No es normal aun

#Normalidad para suero
ad.test( datos_suero$FLORETINA[datos_suero$TIEMPO == "T1"] ) #no es normal aun
ad.test( datos_suero$FLORETINA[datos_suero$TIEMPO == "T6"] ) #No es normal aun
ad.test( datos_suero$FLORETINA[datos_suero$TIEMPO == "T24"] )#No es normal aun
ad.test( datos_suero$FLORETINA[datos_suero$TIEMPO == "T48"] )#No es normal aun

shapiro.test( datos_suero$FLORETINA[datos_suero$TIEMPO == "T1"] ) #no es normal aun
shapiro.test( datos_suero$FLORETINA[datos_suero$TIEMPO == "T6"] ) #No es normal aun
shapiro.test( datos_suero$FLORETINA[datos_suero$TIEMPO == "T24"] ) #No es normal aun
shapiro.test( datos_suero$FLORETINA[datos_suero$TIEMPO == "T48"] )#No es normal aun



#METABOLITO: MORINA
#Normalidad para higado
#Te dejo el codigo, una vez que se completen los datos debería de correr cualquiera de las dos pruebas. De momento no es posible
ad.test( datos_higado$MORINA[datos_higado$TIEMPO == "T1"] ) 
ad.test( datos_higado$MORINA[datos_higado$TIEMPO == "T6"] )
ad.test( datos_higado$MORINA[datos_higado$TIEMPO == "T24"] )
ad.test( datos_higado$MORINA[datos_higado$TIEMPO == "T48"] )
shapiro.test( datos_higado$MORINA[datos_higado$TIEMPO == "T1"] )  #No es normal aun
shapiro.test( datos_higado$MORINA[datos_higado$TIEMPO == "T6"] )  #No es normal aun
shapiro.test( datos_higado$MORINA[datos_higado$TIEMPO == "T24"] )  #No es normal aun
shapiro.test( datos_higado$MORINA[datos_higado$TIEMPO == "T48"] ) #No es normal aun

#Normalidad para orina
ad.test( datos_orina$MORINA[datos_orina$TIEMPO == "T1"] ) #No es normal aun
ad.test( datos_orina$MORINA[datos_orina$TIEMPO == "T6"] ) #No es normal aun
ad.test( datos_orina$MORINA[datos_orina$TIEMPO == "T24"] ) #No es normal aun
ad.test( datos_orina$MORINA[datos_orina$TIEMPO == "T48"] ) #No es normal aun

shapiro.test( datos_orina$MORINA[datos_orina$TIEMPO == "T1"] ) #No es normal aun
shapiro.test( datos_orina$MORINA[datos_orina$TIEMPO == "T6"] )#No es normal aun
shapiro.test( datos_orina$MORINA[datos_orina$TIEMPO == "T24"] ) #no es normal aun
shapiro.test( datos_orina$MORINA[datos_orina$TIEMPO == "T48"] )#No es normal aun

#Normalidad para suero
ad.test( datos_suero$MORINA[datos_suero$TIEMPO == "T1"] ) #no es normal aun
ad.test( datos_suero$MORINA[datos_suero$TIEMPO == "T6"] ) #si es normal 
ad.test( datos_suero$MORINA[datos_suero$TIEMPO == "T24"] )#No es normal aun
ad.test( datos_suero$MORINA[datos_suero$TIEMPO == "T48"] )#si es normal 

shapiro.test( datos_suero$MORINA[datos_suero$TIEMPO == "T1"] ) #no es normal aun
shapiro.test( datos_suero$MORINA[datos_suero$TIEMPO == "T6"] ) #si es normal
shapiro.test( datos_suero$MORINA[datos_suero$TIEMPO == "T24"] ) #No es normal aun
shapiro.test( datos_suero$MORINA[datos_suero$TIEMPO == "T48"] )#si es normal 



#METABOLITO: APIGENINA
#Normalidad para higado
#Te dejo el codigo, una vez que se completen los datos debería de correr cualquiera de las dos pruebas. De momento no es posible
ad.test( datos_higado$APIGENINA[datos_higado$TIEMPO == "T1"] ) 
ad.test( datos_higado$APIGENINA[datos_higado$TIEMPO == "T6"] )
ad.test( datos_higado$APIGENINA[datos_higado$TIEMPO == "T24"] )
ad.test( datos_higado$APIGENINA[datos_higado$TIEMPO == "T48"] )
shapiro.test( datos_higado$APIGENINA[datos_higado$TIEMPO == "T1"] )  
shapiro.test( datos_higado$APIGENINA[datos_higado$TIEMPO == "T6"] )
shapiro.test( datos_higado$APIGENINA[datos_higado$TIEMPO == "T24"] )
shapiro.test( datos_higado$APIGENINA[datos_higado$TIEMPO == "T48"] )

#Normalidad para orina
ad.test( datos_orina$APIGENINA[datos_orina$TIEMPO == "T1"] ) #No es normal aun
ad.test( datos_orina$APIGENINA[datos_orina$TIEMPO == "T6"] ) #No es normal aun
ad.test( datos_orina$APIGENINA[datos_orina$TIEMPO == "T24"] ) #No es normal aun
ad.test( datos_orina$APIGENINA[datos_orina$TIEMPO == "T48"] ) #No es normal aun

shapiro.test( datos_orina$APIGENINA[datos_orina$TIEMPO == "T1"] ) #No es normal aun
shapiro.test( datos_orina$APIGENINA[datos_orina$TIEMPO == "T6"] )#No es normal aun
shapiro.test( datos_orina$APIGENINA[datos_orina$TIEMPO == "T24"] ) #no es normal aun
shapiro.test( datos_orina$APIGENINA[datos_orina$TIEMPO == "T48"] )#No es normal aun

#Normalidad para suero
ad.test( datos_suero$APIGENINA[datos_suero$TIEMPO == "T1"] ) 
ad.test( datos_suero$APIGENINA[datos_suero$TIEMPO == "T6"] ) #No es normal aun
ad.test( datos_suero$APIGENINA[datos_suero$TIEMPO == "T24"] )#No es normal aun
ad.test( datos_suero$APIGENINA[datos_suero$TIEMPO == "T48"] )#No es normal aun

shapiro.test( datos_suero$APIGENINA[datos_suero$TIEMPO == "T1"] ) #no es normal aun
shapiro.test( datos_suero$APIGENINA[datos_suero$TIEMPO == "T6"] ) #No es normal aun
shapiro.test( datos_suero$APIGENINA[datos_suero$TIEMPO == "T24"] ) #No es normal aun
shapiro.test( datos_suero$APIGENINA[datos_suero$TIEMPO == "T48"] )#No es normal aun


#METABOLITO: MIRICETINA
#Normalidad para higado
#Te dejo el codigo, una vez que se completen los datos debería de correr cualquiera de las dos pruebas. De momento no es posible
ad.test( datos_higado$MIRICETINA[datos_higado$TIEMPO == "T1"] ) 
ad.test( datos_higado$MIRICETINA[datos_higado$TIEMPO == "T6"] )
ad.test( datos_higado$MIRICETINA[datos_higado$TIEMPO == "T24"] )
ad.test( datos_higado$MIRICETINA[datos_higado$TIEMPO == "T48"] )
shapiro.test( datos_higado$MIRICETINA[datos_higado$TIEMPO == "T1"] )  
shapiro.test( datos_higado$MIRICETINA[datos_higado$TIEMPO == "T6"] )
shapiro.test( datos_higado$MIRICETINA[datos_higado$TIEMPO == "T24"] )
shapiro.test( datos_higado$MIRICETINA[datos_higado$TIEMPO == "T48"] )

#Normalidad para orina
ad.test( datos_orina$MIRICETINA[datos_orina$TIEMPO == "T1"] ) #No es normal aun
ad.test( datos_orina$MIRICETINA[datos_orina$TIEMPO == "T6"] ) #No es normal aun
ad.test( datos_orina$MIRICETINA[datos_orina$TIEMPO == "T24"] ) #No es normal aun
ad.test( datos_orina$MIRICETINA[datos_orina$TIEMPO == "T48"] ) #No es normal aun

shapiro.test( datos_orina$MIRICETINA[datos_orina$TIEMPO == "T1"] ) #No es normal aun
shapiro.test( datos_orina$MIRICETINA[datos_orina$TIEMPO == "T6"] )#No es normal aun
shapiro.test( datos_orina$MIRICETINA[datos_orina$TIEMPO == "T24"] ) #no es normal aun
shapiro.test( datos_orina$MIRICETINA[datos_orina$TIEMPO == "T48"] )#No es normal aun

#Normalidad para suero
ad.test( datos_suero$MIRICETINA[datos_suero$TIEMPO == "T1"] ) #no es normal aun
ad.test( datos_suero$MIRICETINA[datos_suero$TIEMPO == "T6"] ) #No es normal aun
ad.test( datos_suero$MIRICETINA[datos_suero$TIEMPO == "T24"] )#No es normal aun
ad.test( datos_suero$MIRICETINA[datos_suero$TIEMPO == "T48"] )#No es normal aun

shapiro.test( datos_suero$MIRICETINA[datos_suero$TIEMPO == "T1"] ) #no es normal aun
shapiro.test( datos_suero$MIRICETINA[datos_suero$TIEMPO == "T6"] ) #No es normal aun
shapiro.test( datos_suero$MIRICETINA[datos_suero$TIEMPO == "T24"] ) #No es normal aun
shapiro.test( datos_suero$MIRICETINA[datos_suero$TIEMPO == "T48"] )#No es normal aun



#METABOLITO: ISORHAMNETINA
#Normalidad para higado
#Te dejo el codigo, una vez que se completen los datos debería de correr cualquiera de las dos pruebas. De momento no es posible
ad.test( datos_higado$ISORHAMNETINA[datos_higado$TIEMPO == "T1"] ) 
ad.test( datos_higado$ISORHAMNETINA[datos_higado$TIEMPO == "T6"] )
ad.test( datos_higado$ISORHAMNETINA[datos_higado$TIEMPO == "T24"] )
ad.test( datos_higado$ISORHAMNETINA[datos_higado$TIEMPO == "T48"] )
shapiro.test( datos_higado$ISORHAMNETINA[datos_higado$TIEMPO == "T1"] )  #No es normal aun
shapiro.test( datos_higado$ISORHAMNETINA[datos_higado$TIEMPO == "T6"] )
shapiro.test( datos_higado$ISORHAMNETINA[datos_higado$TIEMPO == "T24"] )
shapiro.test( datos_higado$ISORHAMNETINA[datos_higado$TIEMPO == "T48"] )

#Normalidad para orina
ad.test( datos_orina$ISORHAMNETINA[datos_orina$TIEMPO == "T1"] ) #No es normal aun
ad.test( datos_orina$ISORHAMNETINA[datos_orina$TIEMPO == "T6"] ) #No es normal aun
ad.test( datos_orina$ISORHAMNETINA[datos_orina$TIEMPO == "T24"] ) #No es normal aun
ad.test( datos_orina$ISORHAMNETINA[datos_orina$TIEMPO == "T48"] ) #No es normal aun

shapiro.test( datos_orina$ISORHAMNETINA[datos_orina$TIEMPO == "T1"] ) #No es normal aun
shapiro.test( datos_orina$ISORHAMNETINA[datos_orina$TIEMPO == "T6"] )#No es normal aun
shapiro.test( datos_orina$ISORHAMNETINA[datos_orina$TIEMPO == "T24"] ) #no es normal aun
shapiro.test( datos_orina$ISORHAMNETINA[datos_orina$TIEMPO == "T48"] )#No es normal aun

#Normalidad para suero
ad.test( datos_suero$ISORHAMNETINA[datos_suero$TIEMPO == "T1"] ) #no es normal aun
ad.test( datos_suero$ISORHAMNETINA[datos_suero$TIEMPO == "T6"] ) #No es normal aun
ad.test( datos_suero$ISORHAMNETINA[datos_suero$TIEMPO == "T24"] )#No es normal aun
ad.test( datos_suero$ISORHAMNETINA[datos_suero$TIEMPO == "T48"] )#No es normal aun

shapiro.test( datos_suero$ISORHAMNETINA[datos_suero$TIEMPO == "T1"] ) #no es normal aun
shapiro.test( datos_suero$ISORHAMNETINA[datos_suero$TIEMPO == "T6"] ) #No es normal aun
shapiro.test( datos_suero$ISORHAMNETINA[datos_suero$TIEMPO == "T24"] ) #No es normal aun
shapiro.test( datos_suero$ISORHAMNETINA[datos_suero$TIEMPO == "T48"] )#No es normal aun



#METABOLITO: HESPERIDINA
#Normalidad para higado
#Te dejo el codigo, una vez que se completen los datos debería de correr cualquiera de las dos pruebas. De momento no es posible
ad.test( datos_higado$HESPERIDINA[datos_higado$TIEMPO == "T1"] ) 
ad.test( datos_higado$HESPERIDINA[datos_higado$TIEMPO == "T6"] )
ad.test( datos_higado$HESPERIDINA[datos_higado$TIEMPO == "T24"] )
ad.test( datos_higado$HESPERIDINA[datos_higado$TIEMPO == "T48"] )
shapiro.test( datos_higado$HESPERIDINA[datos_higado$TIEMPO == "T1"] )  
shapiro.test( datos_higado$HESPERIDINA[datos_higado$TIEMPO == "T6"] )
shapiro.test( datos_higado$HESPERIDINA[datos_higado$TIEMPO == "T24"] )
shapiro.test( datos_higado$HESPERIDINA[datos_higado$TIEMPO == "T48"] )

#Normalidad para orina
ad.test( datos_orina$HESPERIDINA[datos_orina$TIEMPO == "T1"] ) #No es normal aun
ad.test( datos_orina$HESPERIDINA[datos_orina$TIEMPO == "T6"] ) #No es normal aun
ad.test( datos_orina$HESPERIDINA[datos_orina$TIEMPO == "T24"] ) #No es normal aun
ad.test( datos_orina$HESPERIDINA[datos_orina$TIEMPO == "T48"] ) #No es normal aun

shapiro.test( datos_orina$HESPERIDINA[datos_orina$TIEMPO == "T1"] ) #No es normal aun
shapiro.test( datos_orina$HESPERIDINA[datos_orina$TIEMPO == "T6"] )#No es normal aun
shapiro.test( datos_orina$HESPERIDINA[datos_orina$TIEMPO == "T24"] ) #no es normal aun
shapiro.test( datos_orina$HESPERIDINA[datos_orina$TIEMPO == "T48"] )#No es normal aun

#Normalidad para suero
ad.test( datos_suero$HESPERIDINA[datos_suero$TIEMPO == "T1"] ) #no es normal aun
ad.test( datos_suero$HESPERIDINA[datos_suero$TIEMPO == "T6"] ) #No es normal aun
ad.test( datos_suero$HESPERIDINA[datos_suero$TIEMPO == "T24"] )#No es normal aun
ad.test( datos_suero$HESPERIDINA[datos_suero$TIEMPO == "T48"] )#No es normal aun

shapiro.test( datos_suero$HESPERIDINA[datos_suero$TIEMPO == "T1"] ) #no es normal aun
shapiro.test( datos_suero$HESPERIDINA[datos_suero$TIEMPO == "T6"] ) #No es normal aun
shapiro.test( datos_suero$HESPERIDINA[datos_suero$TIEMPO == "T24"] ) #No es normal aun
shapiro.test( datos_suero$HESPERIDINA[datos_suero$TIEMPO == "T48"] )#No es normal aun


#METABOLITO: KAEMFEROL
#Normalidad para higado
#Te dejo el codigo, una vez que se completen los datos debería de correr cualquiera de las dos pruebas. De momento no es posible
ad.test( datos_higado$KAEMFEROL[datos_higado$TIEMPO == "T1"] ) 
ad.test( datos_higado$KAEMFEROL[datos_higado$TIEMPO == "T6"] )
ad.test( datos_higado$KAEMFEROL[datos_higado$TIEMPO == "T24"] )
ad.test( datos_higado$KAEMFEROL[datos_higado$TIEMPO == "T48"] )
shapiro.test( datos_higado$KAEMFEROL[datos_higado$TIEMPO == "T1"] )  #No es normal aun
shapiro.test( datos_higado$KAEMFEROL[datos_higado$TIEMPO == "T6"] )
shapiro.test( datos_higado$KAEMFEROL[datos_higado$TIEMPO == "T24"] )
shapiro.test( datos_higado$KAEMFEROL[datos_higado$TIEMPO == "T48"] )

#Normalidad para orina
ad.test( datos_orina$KAEMFEROL[datos_orina$TIEMPO == "T1"] ) #No es normal aun
ad.test( datos_orina$KAEMFEROL[datos_orina$TIEMPO == "T6"] ) #No es normal aun
ad.test( datos_orina$KAEMFEROL[datos_orina$TIEMPO == "T24"] ) #No es normal aun
ad.test( datos_orina$KAEMFEROL[datos_orina$TIEMPO == "T48"] ) #No es normal aun

shapiro.test( datos_orina$KAEMFEROL[datos_orina$TIEMPO == "T1"] ) #No es normal aun
shapiro.test( datos_orina$KAEMFEROL[datos_orina$TIEMPO == "T6"] )#No es normal aun
shapiro.test( datos_orina$KAEMFEROL[datos_orina$TIEMPO == "T24"] ) #no es normal aun
shapiro.test( datos_orina$KAEMFEROL[datos_orina$TIEMPO == "T48"] )#No es normal aun

#Normalidad para suero
ad.test( datos_suero$KAEMFEROL[datos_suero$TIEMPO == "T1"] ) #no es normal aun
ad.test( datos_suero$KAEMFEROL[datos_suero$TIEMPO == "T6"] ) #No es normal aun
ad.test( datos_suero$KAEMFEROL[datos_suero$TIEMPO == "T24"] )#No es normal aun
ad.test( datos_suero$KAEMFEROL[datos_suero$TIEMPO == "T48"] )#No es normal aun

shapiro.test( datos_suero$KAEMFEROL[datos_suero$TIEMPO == "T1"] ) #no es normal aun
shapiro.test( datos_suero$KAEMFEROL[datos_suero$TIEMPO == "T6"] ) #No es normal aun
shapiro.test( datos_suero$KAEMFEROL[datos_suero$TIEMPO == "T24"] ) #No es normal aun
shapiro.test( datos_suero$KAEMFEROL[datos_suero$TIEMPO == "T48"] )#No es normal aun



#METABOLITO: QUERCETINA
#Normalidad para higado
#Te dejo el codigo, una vez que se completen los datos debería de correr cualquiera de las dos pruebas. De momento no es posible
ad.test( datos_higado$QUERCETINA[datos_higado$TIEMPO == "T1"] ) 
ad.test( datos_higado$QUERCETINA[datos_higado$TIEMPO == "T6"] )
ad.test( datos_higado$QUERCETINA[datos_higado$TIEMPO == "T24"] )
ad.test( datos_higado$QUERCETINA[datos_higado$TIEMPO == "T48"] )
shapiro.test( datos_higado$QUERCETINA[datos_higado$TIEMPO == "T1"] ) 
shapiro.test( datos_higado$QUERCETINA[datos_higado$TIEMPO == "T6"] )
shapiro.test( datos_higado$QUERCETINA[datos_higado$TIEMPO == "T24"] )
shapiro.test( datos_higado$QUERCETINA[datos_higado$TIEMPO == "T48"] )

#Normalidad para orina
ad.test( datos_orina$QUERCETINA[datos_orina$TIEMPO == "T1"] ) #No es normal aun
ad.test( datos_orina$QUERCETINA[datos_orina$TIEMPO == "T6"] ) #No es normal aun
ad.test( datos_orina$QUERCETINA[datos_orina$TIEMPO == "T24"] ) #No es normal aun
ad.test( datos_orina$QUERCETINA[datos_orina$TIEMPO == "T48"] ) #No es normal aun

shapiro.test( datos_orina$QUERCETINA[datos_orina$TIEMPO == "T1"] ) #No es normal aun
shapiro.test( datos_orina$QUERCETINA[datos_orina$TIEMPO == "T6"] )#No es normal aun
shapiro.test( datos_orina$QUERCETINA[datos_orina$TIEMPO == "T24"] ) #no es normal aun
shapiro.test( datos_orina$QUERCETINA[datos_orina$TIEMPO == "T48"] )#No es normal aun

#Normalidad para suero
ad.test( datos_suero$QUERCETINA[datos_suero$TIEMPO == "T1"] ) #no es normal aun
ad.test( datos_suero$QUERCETINA[datos_suero$TIEMPO == "T6"] ) #No es normal aun
ad.test( datos_suero$QUERCETINA[datos_suero$TIEMPO == "T24"] )#No es normal aun
ad.test( datos_suero$QUERCETINA[datos_suero$TIEMPO == "T48"] )#No es normal aun

shapiro.test( datos_suero$QUERCETINA[datos_suero$TIEMPO == "T1"] ) #no es normal aun
shapiro.test( datos_suero$QUERCETINA[datos_suero$TIEMPO == "T6"] ) #No es normal aun
shapiro.test( datos_suero$QUERCETINA[datos_suero$TIEMPO == "T24"] ) #No es normal aun
shapiro.test( datos_suero$QUERCETINA[datos_suero$TIEMPO == "T48"] )#No es normal aun



#METABOLITO: CATEQUINA
#Normalidad para higado
#Te dejo el codigo, una vez que se completen los datos debería de correr cualquiera de las dos pruebas. De momento no es posible
ad.test( datos_higado$CATEQUINA[datos_higado$TIEMPO == "T1"] ) 
ad.test( datos_higado$CATEQUINA[datos_higado$TIEMPO == "T6"] )
ad.test( datos_higado$CATEQUINA[datos_higado$TIEMPO == "T24"] )
ad.test( datos_higado$CATEQUINA[datos_higado$TIEMPO == "T48"] )
shapiro.test( datos_higado$CATEQUINA[datos_higado$TIEMPO == "T1"] )  #No es normal aun
shapiro.test( datos_higado$CATEQUINA[datos_higado$TIEMPO == "T6"] )
shapiro.test( datos_higado$CATEQUINA[datos_higado$TIEMPO == "T24"] )
shapiro.test( datos_higado$CATEQUINA[datos_higado$TIEMPO == "T48"] )

#Normalidad para orina
ad.test( datos_orina$CATEQUINA[datos_orina$TIEMPO == "T1"] ) #No es normal aun
ad.test( datos_orina$CATEQUINA[datos_orina$TIEMPO == "T6"] ) #No es normal aun
ad.test( datos_orina$CATEQUINA[datos_orina$TIEMPO == "T24"] ) #No es normal aun
ad.test( datos_orina$CATEQUINA[datos_orina$TIEMPO == "T48"] ) #No es normal aun

shapiro.test( datos_orina$CATEQUINA[datos_orina$TIEMPO == "T1"] ) #No es normal aun
shapiro.test( datos_orina$CATEQUINA[datos_orina$TIEMPO == "T6"] )#No es normal aun
shapiro.test( datos_orina$CATEQUINA[datos_orina$TIEMPO == "T24"] ) #no es normal aun
shapiro.test( datos_orina$CATEQUINA[datos_orina$TIEMPO == "T48"] )#No es normal aun

#Normalidad para suero
ad.test( datos_suero$CATEQUINA[datos_suero$TIEMPO == "T1"] ) #no es normal aun
ad.test( datos_suero$CATEQUINA[datos_suero$TIEMPO == "T6"] ) #No es normal aun
ad.test( datos_suero$CATEQUINA[datos_suero$TIEMPO == "T24"] )#No es normal aun
ad.test( datos_suero$CATEQUINA[datos_suero$TIEMPO == "T48"] )#No es normal aun

shapiro.test( datos_suero$CATEQUINA[datos_suero$TIEMPO == "T1"] ) #no es normal aun
shapiro.test( datos_suero$CATEQUINA[datos_suero$TIEMPO == "T6"] ) #No es normal aun
shapiro.test( datos_suero$CATEQUINA[datos_suero$TIEMPO == "T24"] ) #No es normal aun
shapiro.test( datos_suero$CATEQUINA[datos_suero$TIEMPO == "T48"] )#No es normal aun


#METABOLITO: FLORITZINA
#Normalidad para higado
#Te dejo el codigo, una vez que se completen los datos debería de correr cualquiera de las dos pruebas. De momento no es posible
ad.test( datos_higado$FLORITZINA[datos_higado$TIEMPO == "T1"] ) 
ad.test( datos_higado$FLORITZINA[datos_higado$TIEMPO == "T6"] )
ad.test( datos_higado$FLORITZINA[datos_higado$TIEMPO == "T24"] )
ad.test( datos_higado$FLORITZINA[datos_higado$TIEMPO == "T48"] )
shapiro.test( datos_higado$FLORITZINA[datos_higado$TIEMPO == "T1"] )  #No es normal aun
shapiro.test( datos_higado$FLORITZINA[datos_higado$TIEMPO == "T6"] )
shapiro.test( datos_higado$FLORITZINA[datos_higado$TIEMPO == "T24"] )
shapiro.test( datos_higado$FLORITZINA[datos_higado$TIEMPO == "T48"] )

#Normalidad para orina
ad.test( datos_orina$FLORITZINA[datos_orina$TIEMPO == "T1"] ) #No es normal aun
ad.test( datos_orina$FLORITZINA[datos_orina$TIEMPO == "T6"] ) #No es normal aun
ad.test( datos_orina$FLORITZINA[datos_orina$TIEMPO == "T24"] ) #No es normal aun
ad.test( datos_orina$FLORITZINA[datos_orina$TIEMPO == "T48"] ) #No es normal aun

shapiro.test( datos_orina$FLORITZINA[datos_orina$TIEMPO == "T1"] ) #No es normal aun
shapiro.test( datos_orina$FLORITZINA[datos_orina$TIEMPO == "T6"] )#No es normal aun
shapiro.test( datos_orina$FLORITZINA[datos_orina$TIEMPO == "T24"] ) #no es normal aun
shapiro.test( datos_orina$FLORITZINA[datos_orina$TIEMPO == "T48"] )#No es normal aun

#Normalidad para suero
ad.test( datos_suero$FLORITZINA[datos_suero$TIEMPO == "T1"] ) #no es normal aun
ad.test( datos_suero$FLORITZINA[datos_suero$TIEMPO == "T6"] ) #No es normal aun
ad.test( datos_suero$FLORITZINA[datos_suero$TIEMPO == "T24"] )#No es normal aun
ad.test( datos_suero$FLORITZINA[datos_suero$TIEMPO == "T48"] )#No es normal aun

shapiro.test( datos_suero$FLORITZINA[datos_suero$TIEMPO == "T1"] ) #no es normal aun
shapiro.test( datos_suero$FLORITZINA[datos_suero$TIEMPO == "T6"] ) #No es normal aun
shapiro.test( datos_suero$FLORITZINA[datos_suero$TIEMPO == "T24"] ) #No es normal aun
shapiro.test( datos_suero$FLORITZINA[datos_suero$TIEMPO == "T48"] )#No es normal aun



#METABOLITO: CUCURBITACINA.D
#Normalidad para higado
#Te dejo el codigo, una vez que se completen los datos debería de correr cualquiera de las dos pruebas. De momento no es posible
ad.test( datos_higado$CUCURBITACINA.D[datos_higado$TIEMPO == "T1"] ) #No es normal aun
ad.test( datos_higado$CUCURBITACINA.D[datos_higado$TIEMPO == "T6"] ) #Si es normal aun
ad.test( datos_higado$CUCURBITACINA.D[datos_higado$TIEMPO == "T24"] ) #No es normal aun
ad.test( datos_higado$CUCURBITACINA.D[datos_higado$TIEMPO == "T48"] )#No es normal aun
shapiro.test( datos_higado$CUCURBITACINA.D[datos_higado$TIEMPO == "T1"] )  #No es normal aun
shapiro.test( datos_higado$CUCURBITACINA.D[datos_higado$TIEMPO == "T6"] )#No es normal aun
shapiro.test( datos_higado$CUCURBITACINA.D[datos_higado$TIEMPO == "T24"] )#No es normal aun
shapiro.test( datos_higado$CUCURBITACINA.D[datos_higado$TIEMPO == "T48"] )#No es normal aun

#Normalidad para orina
ad.test( datos_orina$CUCURBITACINA.D[datos_orina$TIEMPO == "T1"] ) 
ad.test( datos_orina$CUCURBITACINA.D[datos_orina$TIEMPO == "T6"] ) 
ad.test( datos_orina$CUCURBITACINA.D[datos_orina$TIEMPO == "T24"] ) 
ad.test( datos_orina$CUCURBITACINA.D[datos_orina$TIEMPO == "T48"] ) 

shapiro.test( datos_orina$CUCURBITACINA.D[datos_orina$TIEMPO == "T1"] ) 
shapiro.test( datos_orina$CUCURBITACINA.D[datos_orina$TIEMPO == "T6"] )
shapiro.test( datos_orina$CUCURBITACINA.D[datos_orina$TIEMPO == "T24"] )
shapiro.test( datos_orina$CUCURBITACINA.D[datos_orina$TIEMPO == "T48"] )

#Normalidad para suero
ad.test( datos_suero$CUCURBITACINA.D[datos_suero$TIEMPO == "T1"] ) #no es normal aun
ad.test( datos_suero$CUCURBITACINA.D[datos_suero$TIEMPO == "T6"] ) #Si es normal aun
ad.test( datos_suero$CUCURBITACINA.D[datos_suero$TIEMPO == "T24"] )#No es normal aun
ad.test( datos_suero$CUCURBITACINA.D[datos_suero$TIEMPO == "T48"] )#No es normal aun

shapiro.test( datos_suero$CUCURBITACINA.D[datos_suero$TIEMPO == "T1"] ) #no es normal aun
shapiro.test( datos_suero$CUCURBITACINA.D[datos_suero$TIEMPO == "T6"] ) #No es normal aun
shapiro.test( datos_suero$CUCURBITACINA.D[datos_suero$TIEMPO == "T24"] ) #No es normal aun
shapiro.test( datos_suero$CUCURBITACINA.D[datos_suero$TIEMPO == "T48"] )#No es normal aun



#METABOLITO: CUCURBITACINA.I
#Normalidad para higado
#Te dejo el codigo, una vez que se completen los datos debería de correr cualquiera de las dos pruebas. De momento no es posible
ad.test( datos_higado$CUCURBITACINA.I[datos_higado$TIEMPO == "T1"] ) #No es normal aun
ad.test( datos_higado$CUCURBITACINA.I[datos_higado$TIEMPO == "T6"] )#Si es normal aun
ad.test( datos_higado$CUCURBITACINA.I[datos_higado$TIEMPO == "T24"] )#No es normal aun
ad.test( datos_higado$CUCURBITACINA.I[datos_higado$TIEMPO == "T48"] )#No es normal aun
shapiro.test( datos_higado$CUCURBITACINA.I[datos_higado$TIEMPO == "T1"] )  #No es normal aun
shapiro.test( datos_higado$CUCURBITACINA.I[datos_higado$TIEMPO == "T6"] ) #Si es normal
shapiro.test( datos_higado$CUCURBITACINA.I[datos_higado$TIEMPO == "T24"] )#No es normal aun
shapiro.test( datos_higado$CUCURBITACINA.I[datos_higado$TIEMPO == "T48"] )#No es normal aun

#Normalidad para orina
ad.test( datos_orina$CUCURBITACINA.I[datos_orina$TIEMPO == "T1"] ) 
ad.test( datos_orina$CUCURBITACINA.I[datos_orina$TIEMPO == "T6"] ) 
ad.test( datos_orina$CUCURBITACINA.I[datos_orina$TIEMPO == "T24"] ) 
ad.test( datos_orina$CUCURBITACINA.I[datos_orina$TIEMPO == "T48"] ) 

shapiro.test( datos_orina$CUCURBITACINA.I[datos_orina$TIEMPO == "T1"] ) 
shapiro.test( datos_orina$CUCURBITACINA.I[datos_orina$TIEMPO == "T6"] )
shapiro.test( datos_orina$CUCURBITACINA.I[datos_orina$TIEMPO == "T24"] ) 
shapiro.test( datos_orina$CUCURBITACINA.I[datos_orina$TIEMPO == "T48"] )

#Normalidad para suero
ad.test( datos_suero$CUCURBITACINA.I[datos_suero$TIEMPO == "T1"] ) #no es normal aun
ad.test( datos_suero$CUCURBITACINA.I[datos_suero$TIEMPO == "T6"] ) #sI es normal aun
ad.test( datos_suero$CUCURBITACINA.I[datos_suero$TIEMPO == "T24"] )#No es normal aun
ad.test( datos_suero$CUCURBITACINA.I[datos_suero$TIEMPO == "T48"] )#No es normal aun

shapiro.test( datos_suero$CUCURBITACINA.I[datos_suero$TIEMPO == "T1"] ) #no es normal aun
shapiro.test( datos_suero$CUCURBITACINA.I[datos_suero$TIEMPO == "T6"] ) #SI es normal aun
shapiro.test( datos_suero$CUCURBITACINA.I[datos_suero$TIEMPO == "T24"] ) #No es normal aun
shapiro.test( datos_suero$CUCURBITACINA.I[datos_suero$TIEMPO == "T48"] )#No es normal aun



#METABOLITO: CUCURBITACINA.IIA
#Normalidad para higado
#Te dejo el codigo, una vez que se completen los datos debería de correr cualquiera de las dos pruebas. De momento no es posible
ad.test( datos_higado$CUCURBITACINA.IIA[datos_higado$TIEMPO == "T1"] ) #No es normal aun
ad.test( datos_higado$CUCURBITACINA.IIA[datos_higado$TIEMPO == "T6"] )#No es normal aun
ad.test( datos_higado$CUCURBITACINA.IIA[datos_higado$TIEMPO == "T24"] )#No es normal aun
ad.test( datos_higado$CUCURBITACINA.IIA[datos_higado$TIEMPO == "T48"] )#No es normal aun
shapiro.test( datos_higado$CUCURBITACINA.IIA[datos_higado$TIEMPO == "T1"] )  #No es normal aun
shapiro.test( datos_higado$CUCURBITACINA.IIA[datos_higado$TIEMPO == "T6"] )#No es normal aun
shapiro.test( datos_higado$CUCURBITACINA.IIA[datos_higado$TIEMPO == "T24"] )#No es normal aun
shapiro.test( datos_higado$CUCURBITACINA.IIA[datos_higado$TIEMPO == "T48"] )#No es normal aun

#Normalidad para orina
ad.test( datos_orina$CUCURBITACINA.IIA[datos_orina$TIEMPO == "T1"] ) 
ad.test( datos_orina$CUCURBITACINA.IIA[datos_orina$TIEMPO == "T6"] ) 
ad.test( datos_orina$CUCURBITACINA.IIA[datos_orina$TIEMPO == "T24"] ) 
ad.test( datos_orina$CUCURBITACINA.IIA[datos_orina$TIEMPO == "T48"] ) 

shapiro.test( datos_orina$CUCURBITACINA.IIA[datos_orina$TIEMPO == "T1"] ) 
shapiro.test( datos_orina$CUCURBITACINA.IIA[datos_orina$TIEMPO == "T6"] )
shapiro.test( datos_orina$CUCURBITACINA.IIA[datos_orina$TIEMPO == "T24"] ) 
shapiro.test( datos_orina$CUCURBITACINA.IIA[datos_orina$TIEMPO == "T48"] )

#Normalidad para suero
ad.test( datos_suero$CUCURBITACINA.IIA[datos_suero$TIEMPO == "T1"] ) #no es normal aun
ad.test( datos_suero$CUCURBITACINA.IIA[datos_suero$TIEMPO == "T6"] ) #No es normal aun
ad.test( datos_suero$CUCURBITACINA.IIA[datos_suero$TIEMPO == "T24"] )#No es normal aun
ad.test( datos_suero$CUCURBITACINA.IIA[datos_suero$TIEMPO == "T48"] )#No es normal aun

shapiro.test( datos_suero$CUCURBITACINA.IIA[datos_suero$TIEMPO == "T1"] ) #no es normal aun
shapiro.test( datos_suero$CUCURBITACINA.IIA[datos_suero$TIEMPO == "T6"] ) #No es normal aun
shapiro.test( datos_suero$CUCURBITACINA.IIA[datos_suero$TIEMPO == "T24"] ) #No es normal aun
shapiro.test( datos_suero$CUCURBITACINA.IIA[datos_suero$TIEMPO == "T48"] )#No es normal aun



#METABOLITO: CUCURBITACINA.B
#Normalidad para higado
#Te dejo el codigo, una vez que se completen los datos debería de correr cualquiera de las dos pruebas. De momento no es posible
ad.test( datos_higado$CUCURBITACINA.B[datos_higado$TIEMPO == "T1"] ) #Si es normal
ad.test( datos_higado$CUCURBITACINA.B[datos_higado$TIEMPO == "T6"] )#No es normal aun
ad.test( datos_higado$CUCURBITACINA.B[datos_higado$TIEMPO == "T24"] ) #Si es normal
ad.test( datos_higado$CUCURBITACINA.B[datos_higado$TIEMPO == "T48"] ) #Si es normal
shapiro.test( datos_higado$CUCURBITACINA.B[datos_higado$TIEMPO == "T1"] )  #No es normal aun
shapiro.test( datos_higado$CUCURBITACINA.B[datos_higado$TIEMPO == "T6"] )#Si es normal
shapiro.test( datos_higado$CUCURBITACINA.B[datos_higado$TIEMPO == "T24"] )#Si es normal
shapiro.test( datos_higado$CUCURBITACINA.B[datos_higado$TIEMPO == "T48"] )#Si es normal

#Normalidad para orina
ad.test( datos_orina$CUCURBITACINA.B[datos_orina$TIEMPO == "T1"] ) 
ad.test( datos_orina$CUCURBITACINA.B[datos_orina$TIEMPO == "T6"] ) 
ad.test( datos_orina$CUCURBITACINA.B[datos_orina$TIEMPO == "T24"] ) 
ad.test( datos_orina$CUCURBITACINA.B[datos_orina$TIEMPO == "T48"] ) 

shapiro.test( datos_orina$CUCURBITACINA.B[datos_orina$TIEMPO == "T1"] ) 
shapiro.test( datos_orina$CUCURBITACINA.B[datos_orina$TIEMPO == "T6"] )
shapiro.test( datos_orina$CUCURBITACINA.B[datos_orina$TIEMPO == "T24"] ) 
shapiro.test( datos_orina$CUCURBITACINA.B[datos_orina$TIEMPO == "T48"] )

#Normalidad para suero
ad.test( datos_suero$CUCURBITACINA.B[datos_suero$TIEMPO == "T1"] ) #no es normal aun
ad.test( datos_suero$CUCURBITACINA.B[datos_suero$TIEMPO == "T6"] ) #No es normal aun
ad.test( datos_suero$CUCURBITACINA.B[datos_suero$TIEMPO == "T24"] )#Si es normal 
ad.test( datos_suero$CUCURBITACINA.B[datos_suero$TIEMPO == "T48"] )#Si es normal 

shapiro.test( datos_suero$CUCURBITACINA.B[datos_suero$TIEMPO == "T1"] ) #no es normal aun
shapiro.test( datos_suero$CUCURBITACINA.B[datos_suero$TIEMPO == "T6"] ) #si es normal 
shapiro.test( datos_suero$CUCURBITACINA.B[datos_suero$TIEMPO == "T24"] ) #si es normal 
shapiro.test( datos_suero$CUCURBITACINA.B[datos_suero$TIEMPO == "T48"] )#si es normal 



#METABOLITO: CUCURBITACINA.E
#Normalidad para higado
#Te dejo el codigo, una vez que se completen los datos debería de correr cualquiera de las dos pruebas. De momento no es posible
ad.test( datos_higado$CUCURBITACINA.E[datos_higado$TIEMPO == "T1"] ) #No es normal aun
ad.test( datos_higado$CUCURBITACINA.E[datos_higado$TIEMPO == "T6"] )#No es normal aun
ad.test( datos_higado$CUCURBITACINA.E[datos_higado$TIEMPO == "T24"] )#No es normal aun
ad.test( datos_higado$CUCURBITACINA.E[datos_higado$TIEMPO == "T48"] )#SI es normal aun
shapiro.test( datos_higado$CUCURBITACINA.E[datos_higado$TIEMPO == "T1"] )  #No es normal aun
shapiro.test( datos_higado$CUCURBITACINA.E[datos_higado$TIEMPO == "T6"] )#No es normal aun
shapiro.test( datos_higado$CUCURBITACINA.E[datos_higado$TIEMPO == "T24"] )#No es normal aun
shapiro.test( datos_higado$CUCURBITACINA.E[datos_higado$TIEMPO == "T48"] ) #No es normal aun

#Normalidad para orina
ad.test( datos_orina$CUCURBITACINA.E[datos_orina$TIEMPO == "T1"] )
ad.test( datos_orina$CUCURBITACINA.E[datos_orina$TIEMPO == "T6"] )
ad.test( datos_orina$CUCURBITACINA.E[datos_orina$TIEMPO == "T24"] ) 
ad.test( datos_orina$CUCURBITACINA.E[datos_orina$TIEMPO == "T48"] ) 

shapiro.test( datos_orina$CUCURBITACINA.E[datos_orina$TIEMPO == "T1"] ) 
shapiro.test( datos_orina$CUCURBITACINA.E[datos_orina$TIEMPO == "T6"] )
shapiro.test( datos_orina$CUCURBITACINA.E[datos_orina$TIEMPO == "T24"] ) 
shapiro.test( datos_orina$CUCURBITACINA.E[datos_orina$TIEMPO == "T48"] )

#Normalidad para suero
ad.test( datos_suero$CUCURBITACINA.E[datos_suero$TIEMPO == "T1"] ) #no es normal aun
ad.test( datos_suero$CUCURBITACINA.E[datos_suero$TIEMPO == "T6"] ) #No es normal aun
ad.test( datos_suero$CUCURBITACINA.E[datos_suero$TIEMPO == "T24"] )#No es normal aun
ad.test( datos_suero$CUCURBITACINA.E[datos_suero$TIEMPO == "T48"] )#Si es normal aun

shapiro.test( datos_suero$CUCURBITACINA.E[datos_suero$TIEMPO == "T1"] ) #no es normal aun
shapiro.test( datos_suero$CUCURBITACINA.E[datos_suero$TIEMPO == "T6"] ) #No es normal aun
shapiro.test( datos_suero$CUCURBITACINA.E[datos_suero$TIEMPO == "T24"] ) #No es normal aun
shapiro.test( datos_suero$CUCURBITACINA.E[datos_suero$TIEMPO == "T48"] )#No es normal aun


#------------------------------------------------------
# 5. TRANSFORMACION
#------------------------------------------------------
#Nota. Este paso solo puede realizarse una vez que tienes todos tus datos

#------------------------------------------------------
# 6. HOMOCEDASTICIDAD
#------------------------------------------------------
#Nota. Esta puede cambiar cuando se complete tu base
#Aun no se pueden estimar varios metabolitos hasta q esten completos
#Prueba de hipotesis
#Ho: Los datos tienen homogeneidad de varianza
#Ha: lo contrario
#Interpretación: 
#Con un p-value >0.05,no podemos rechazar la hipótesis nula. Por lo tanto suponemos homogeneidad de varianzas.

#Higado
bartlett.test(RUTINA~TRATAMIENTO,data=datos_higado)
bartlett.test(NARINGENINA~TRATAMIENTO,data=datos_higado)
bartlett.test(FLORETINA~TRATAMIENTO,data=datos_higado) #No hay homogeneidad de varianza
bartlett.test(MORINA~TRATAMIENTO,data=datos_higado)
bartlett.test(APIGENINA~TRATAMIENTO,data=datos_higado)
bartlett.test(MIRICETINA~TRATAMIENTO,data=datos_higado)
bartlett.test(ISORHAMNETINA~TRATAMIENTO,data=datos_higado)
bartlett.test(HESPERIDINA~TRATAMIENTO,data=datos_higado)
bartlett.test(KAEMFEROL~TRATAMIENTO,data=datos_higado)
bartlett.test(QUERCETINA~TRATAMIENTO,data=datos_higado)
bartlett.test(CATEQUINA~TRATAMIENTO,data=datos_higado)
bartlett.test(FLORITZINA~TRATAMIENTO,data=datos_higado)
bartlett.test(CUCURBITACINA.D~TRATAMIENTO,data=datos_higado) #Si hay homogeneidad de varianza
bartlett.test(CUCURBITACINA.I~TRATAMIENTO,data=datos_higado) #No hay homogeneidad de varianza
bartlett.test(CUCURBITACINA.IIA~TRATAMIENTO,data=datos_higado) #No hay homogeneidad de varianza
bartlett.test(CUCURBITACINA.B~TRATAMIENTO,data=datos_higado)#Si hay homogeneidad de varianza
bartlett.test(CUCURBITACINA.E~TRATAMIENTO,data=datos_higado)#No hay homogeneidad de varianza

#Orina
bartlett.test(RUTINA~TRATAMIENTO,data=datos_orina)#No hay homogeneidad de varianza
bartlett.test(NARINGENINA~TRATAMIENTO,data=datos_orina)#Si hay homogeneidad de varianza
bartlett.test(FLORETINA~TRATAMIENTO,data=datos_orina) #si hay homogeneidad de varianza
bartlett.test(MORINA~TRATAMIENTO,data=datos_orina)#No hay homogeneidad de varianza
bartlett.test(APIGENINA~TRATAMIENTO,data=datos_orina)#No hay homogeneidad de varianza
bartlett.test(MIRICETINA~TRATAMIENTO,data=datos_orina)#No hay homogeneidad de varianza
bartlett.test(ISORHAMNETINA~TRATAMIENTO,data=datos_orina)#No hay homogeneidad de varianza
bartlett.test(HESPERIDINA~TRATAMIENTO,data=datos_orina)#No hay homogeneidad de varianza
bartlett.test(KAEMFEROL~TRATAMIENTO,data=datos_orina)#No hay homogeneidad de varianza
bartlett.test(QUERCETINA~TRATAMIENTO,data=datos_orina)#si hay homogeneidad de varianza
bartlett.test(CATEQUINA~TRATAMIENTO,data=datos_orina)#No hay homogeneidad de varianza
bartlett.test(FLORITZINA~TRATAMIENTO,data=datos_orina)#No hay homogeneidad de varianza
bartlett.test(CUCURBITACINA.D~TRATAMIENTO,data=datos_orina) 
bartlett.test(CUCURBITACINA.I~TRATAMIENTO,data=datos_orina) 
bartlett.test(CUCURBITACINA.IIA~TRATAMIENTO,data=datos_orina) 
bartlett.test(CUCURBITACINA.B~TRATAMIENTO,data=datos_orina)
bartlett.test(CUCURBITACINA.E~TRATAMIENTO,data=datos_orina)


#Suero
bartlett.test(RUTINA~TRATAMIENTO,data=datos_suero)#No hay homogeneidad de varianza
bartlett.test(NARINGENINA~TRATAMIENTO,data=datos_suero)#Si hay homogeneidad de varianza
bartlett.test(FLORETINA~TRATAMIENTO,data=datos_suero) #si hay homogeneidad de varianza
bartlett.test(MORINA~TRATAMIENTO,data=datos_suero)#si hay homogeneidad de varianza
bartlett.test(APIGENINA~TRATAMIENTO,data=datos_suero)#si hay homogeneidad de varianza
bartlett.test(MIRICETINA~TRATAMIENTO,data=datos_suero)#No hay homogeneidad de varianza
bartlett.test(ISORHAMNETINA~TRATAMIENTO,data=datos_suero)#si hay homogeneidad de varianza
bartlett.test(HESPERIDINA~TRATAMIENTO,data=datos_suero)#No hay homogeneidad de varianza
bartlett.test(KAEMFEROL~TRATAMIENTO,data=datos_suero)#si hay homogeneidad de varianza
bartlett.test(QUERCETINA~TRATAMIENTO,data=datos_suero)#no hay homogeneidad de varianza
bartlett.test(CATEQUINA~TRATAMIENTO,data=datos_suero)#No hay homogeneidad de varianza
bartlett.test(FLORITZINA~TRATAMIENTO,data=datos_suero)#No hay homogeneidad de varianza
bartlett.test(CUCURBITACINA.D~TRATAMIENTO,data=datos_suero) #No hay homogeneidad de varianza
bartlett.test(CUCURBITACINA.I~TRATAMIENTO,data=datos_suero) #No hay homogeneidad de varianza
bartlett.test(CUCURBITACINA.IIA~TRATAMIENTO,data=datos_suero) #No hay homogeneidad de varianza
bartlett.test(CUCURBITACINA.B~TRATAMIENTO,data=datos_suero)#si hay homogeneidad de varianza
bartlett.test(CUCURBITACINA.E~TRATAMIENTO,data=datos_suero)#No hay homogeneidad de varianza



#------------------------------------------------------
# 6. SELECCION DE ANOVAS
#------------------------------------------------------
library("ggpubr")
library(dplyr)
library(multcomp)
library(car)
#Debido a que tienes dos factores: Diferencias entre TIEMPOS, pero tambien entre TRATAMIENTOS
#Utilizamos un anova de dos vías
#NOTA. Aqui estoy corriendo asumiendo que tus datos alcanzaran la normalidad. Pero esto puede cambiar cuando completes toda la información
#Asi que los resultados no son definitivos aún

#------------------------------------------------------
# 6.1 ANOVA DE UNA VIA + PAIRWISE TEST + TUKEY TEST
#------------------------------------------------------


#Primero veamos si hay diferencias entre órganos únicamente
#Anova de una via (One-way ANOVA): examina la igualdad de las medias de la población
#para un resultado cuantitativo y una única variable categórica con dos o más niveles.
#La variable dependiente es cuantitativa: peso de cada metabolito
#Factor: la variable categorica que define los grupos a comparar
  #en este caso tratamiento con 5 niveles : control, vehiculo, 8, 125 y 250 mg/mL


#En ANOVA DE UNA VIA: se debe comprobar el supuesto de independencia, normalidad y homocedasticidad
names(datos)

#Prueba de hipotesis
#Ho: No hay diferencias significativas entre organos
#Ha: Si hay diferenicas significativas entre organos

#Interpretación: 
#Con un p-value menor de 0.05, podemos rechazar la hipótesis nula. Se acepta la hipótesis alternativa de que si hay diferencia en al menos una de las medias de los grupos de órganos 
#Es decir, los órganos tienen un efecto significativo en el el peso de cada metabolito.

#METABOLITO: RUTINA

#Anova 1 via:
modelo1_rutina=aov(RUTINA~ORGANOS,data = datos)
summary(modelo1_rutina)
#Hay diferencias significativas entre organos para el metabolito RUTINA


#Post-hoc procedimiento:
#Para conocer que grupos de edad son los que difieren, se deben realizar contrastes dos a dos
#Procedimiento posthoc contrasta diferencias de todos los grupos e identifica aquellas diferencias estadisticamente significativas
pairwise.t.test( datos$RUTINA, 
                 datos$ORGANOS, 
                 p.adj = "bonferroni")

#Interpretación: 
#Existen diferencias significativas (p-valor < 0.05) entre: Suero  e Higado (0.025) para el metabolito Rutina
#Existen diferencias significativas entre suero y orina (3.2e-09) para el metabolito Rutina
#No hay diferencia significativa entre orina e higado para el metabolito Rutina (1.000)

#Prueba de medias TUKEY #Es la misma que el procedimiento POSTHOC pero mas sencillo de entender
#Como la prueba ANOVA es significativa, podemos calcular Tukey HSD (Diferencias significativas honestas de Tukey, función R: TukeyHSD()) 
#para realizar múltiples comparaciones por pares entre las medias de los grupos.
#Las diferencias seran significativas cuando el pvalue sea menor a 0.05
TukeyHSD(modelo1_rutina)

#Eso significa
#diff: difference between means of the two groups
#lwr, upr: the lower and the upper end point of the confidence interval at 95% (default)
#p adj: p-value after adjustment for the multiple comparisons.

#REVISIÓN DEL MODELO ANOVA
jpeg(filename = "Diagnostico_Anova_1via_RUTINA.jpeg",width = 190,height = 130,units = "mm",res = 300) #No cambiar 190 
par(mfrow=c(2,2))
plot(modelo1_rutina)
dev.off()

#Cumplimiento de supuestos
#Homogeneidad de varianza
leveneTest(RUTINA ~ ORGANOS, data = datos) #Veamos como cambia esta una vez que tienes todos tus datos
shapiro.test(residuals(modelo1_rutina)) #Veamos como cambia esta una vez que tienes todos tus datos



#METABOLITO: NARINGENINA

#Anova 1 via:
modelo1_NARINGENINA=aov(NARINGENINA~ORGANOS,data = datos)
summary(modelo1_NARINGENINA)
#Hay diferencias significativas entre organos para el metabolito NARINGENINA


#Post-hoc procedimiento:
#Para conocer que grupos de edad son los que difieren, se deben realizar contrastes dos a dos
#Procedimiento posthoc contrasta diferencias de todos los grupos e identifica aquellas diferencias estadisticamente significativas
pairwise.t.test( datos$NARINGENINA, 
                 datos$ORGANOS, 
                 p.adj = "bonferroni")

#Interpretación: 
#Existen diferencias significativas (p-valor < 0.05) entre: 
#Orina y Higado (5.9e-07)
#Suero y Higado (5.2e-05)
#No hay diferencias significativas entre suero y orina (0.079)


#Prueba de medias TUKEY #Es la misma que el procedimiento POSTHOC pero mas sencillo de entender
#Como la prueba ANOVA es significativa, podemos calcular Tukey HSD (Diferencias significativas honestas de Tukey, función R: TukeyHSD()) 
#para realizar múltiples comparaciones por pares entre las medias de los grupos.
#Las diferencias seran significativas cuando el pvalue sea menor a 0.05
TukeyHSD(modelo1_NARINGENINA)

#Eso significa
#diff: difference between means of the two groups
#lwr, upr: the lower and the upper end point of the confidence interval at 95% (default)
#p adj: p-value after adjustment for the multiple comparisons.

#REVISIÓN DEL MODELO ANOVA
jpeg(filename = "Diagnostico_Anova_1via_NARINGENINA.jpeg",width = 190,height = 130,units = "mm",res = 300) #No cambiar 190 
par(mfrow=c(2,2))
plot(modelo1_NARINGENINA)
dev.off()

#Cumplimiento de supuestos
#Homogeneidad de varianza
leveneTest(NARINGENINA ~ ORGANOS, data = datos) #Veamos como cambia esta una vez que tienes todos tus datos
shapiro.test(residuals(modelo1_NARINGENINA)) #Veamos como cambia esta una vez que tienes todos tus datos



#METABOLITO: FLORETINA

#Anova 1 via:
modelo1_FLORETINA=aov(FLORETINA~ORGANOS,data = datos)
summary(modelo1_FLORETINA)
#Hay diferencias significativas entre organos para el metabolito FLORETINA


#Post-hoc procedimiento:
#Para conocer que grupos de edad son los que difieren, se deben realizar contrastes dos a dos
#Procedimiento posthoc contrasta diferencias de todos los grupos e identifica aquellas diferencias estadisticamente significativas
pairwise.t.test( datos$FLORETINA, 
                 datos$ORGANOS, 
                 p.adj = "bonferroni")

#Interpretación: 
#Existen diferencias significativas (p-valor < 0.05) entre: 
#Orina y Higado (2.2e-09)
#Suero y Higado (0.027 )
#Suero y orina (1.9e-06)


#Prueba de medias TUKEY #Es la misma que el procedimiento POSTHOC pero mas sencillo de entender
#Como la prueba ANOVA es significativa, podemos calcular Tukey HSD (Diferencias significativas honestas de Tukey, función R: TukeyHSD()) 
#para realizar múltiples comparaciones por pares entre las medias de los grupos.
#Las diferencias seran significativas cuando el pvalue sea menor a 0.05
TukeyHSD(modelo1_FLORETINA)

#Eso significa
#diff: difference between means of the two groups
#lwr, upr: the lower and the upper end point of the confidence interval at 95% (default)
#p adj: p-value after adjustment for the multiple comparisons.

#REVISIÓN DEL MODELO ANOVA
jpeg(filename = "Diagnostico_Anova_1via_FLORETINA.jpeg",width = 190,height = 130,units = "mm",res = 300) #No cambiar 190 
par(mfrow=c(2,2))
plot(modelo1_FLORETINA)
dev.off()

#Cumplimiento de supuestos
#Homogeneidad de varianza
leveneTest(FLORETINA ~ ORGANOS, data = datos) #Veamos como cambia esta una vez que tienes todos tus datos
shapiro.test(residuals(modelo1_FLORETINA)) #Veamos como cambia esta una vez que tienes todos tus datos


#METABOLITO: MORINA

#Anova 1 via:
modelo1_MORINA=aov(MORINA~ORGANOS,data = datos)
summary(modelo1_MORINA)
#No Hay diferencias significativas entre organos para el metabolito MORINA


#Te dejo el código por si cambia la significancia cuando agregues los datos, pero cómo el anova no salio significativo, estas pruebas no son necesarias

#Post-hoc procedimiento:
#Para conocer que grupos de edad son los que difieren, se deben realizar contrastes dos a dos
#Procedimiento posthoc contrasta diferencias de todos los grupos e identifica aquellas diferencias estadisticamente significativas
pairwise.t.test( datos$MORINA, 
                 datos$ORGANOS, 
                 p.adj = "bonferroni")

#Interpretación: 
#No Existen diferencias significativas (p-valor < 0.05) entre ningun organo 



#Prueba de medias TUKEY #Es la misma que el procedimiento POSTHOC pero mas sencillo de entender
#Como la prueba ANOVA es significativa, podemos calcular Tukey HSD (Diferencias significativas honestas de Tukey, función R: TukeyHSD()) 
#para realizar múltiples comparaciones por pares entre las medias de los grupos.
#Las diferencias seran significativas cuando el pvalue sea menor a 0.05
TukeyHSD(modelo1_MORINA)

#Eso significa
#diff: difference between means of the two groups
#lwr, upr: the lower and the upper end point of the confidence interval at 95% (default)
#p adj: p-value after adjustment for the multiple comparisons.

#REVISIÓN DEL MODELO ANOVA
jpeg(filename = "Diagnostico_Anova_1via_MORINA.jpeg",width = 190,height = 130,units = "mm",res = 300) #No cambiar 190 
par(mfrow=c(2,2))
plot(modelo1_MORINA)
dev.off()

#Cumplimiento de supuestos
#Homogeneidad de varianza
leveneTest(MORINA ~ ORGANOS, data = datos) #Veamos como cambia esta una vez que tienes todos tus datos - Se cumple el supuesto en este
shapiro.test(residuals(modelo1_MORINA)) #Veamos como cambia esta una vez que tienes todos tus datos



#METABOLITO: APIGENINA

#Anova 1 via:
modelo1_APIGENINA=aov(APIGENINA~ORGANOS,data = datos)
summary(modelo1_APIGENINA)
#Hay diferencias significativas entre organos para el metabolito APIGENINA


#Post-hoc procedimiento:
#Para conocer que grupos de edad son los que difieren, se deben realizar contrastes dos a dos
#Procedimiento posthoc contrasta diferencias de todos los grupos e identifica aquellas diferencias estadisticamente significativas
pairwise.t.test( datos$APIGENINA, 
                 datos$ORGANOS, 
                 p.adj = "bonferroni")

#Interpretación: 
#Existen diferencias significativas (p-valor < 0.05) entre: 
#Orina e Higado (0.032)
#Suero e Higado (0.034)


#Prueba de medias TUKEY #Es la misma que el procedimiento POSTHOC pero mas sencillo de entender
#Como la prueba ANOVA es significativa, podemos calcular Tukey HSD (Diferencias significativas honestas de Tukey, función R: TukeyHSD()) 
#para realizar múltiples comparaciones por pares entre las medias de los grupos.
#Las diferencias seran significativas cuando el pvalue sea menor a 0.05
TukeyHSD(modelo1_APIGENINA)

#Eso significa
#diff: difference between means of the two groups
#lwr, upr: the lower and the upper end point of the confidence interval at 95% (default)
#p adj: p-value after adjustment for the multiple comparisons.

#REVISIÓN DEL MODELO ANOVA
jpeg(filename = "Diagnostico_Anova_1via_APIGENINA.jpeg",width = 190,height = 130,units = "mm",res = 300) #No cambiar 190 
par(mfrow=c(2,2))
plot(modelo1_APIGENINA)
dev.off()

#Cumplimiento de supuestos
#Homogeneidad de varianza
leveneTest(APIGENINA ~ ORGANOS, data = datos) #Veamos como cambia esta una vez que tienes todos tus datos - de momento se cumple el supuesto
shapiro.test(residuals(modelo1_APIGENINA)) #Veamos como cambia esta una vez que tienes todos tus datos


#METABOLITO: MIRICETINA

#Anova 1 via:
modelo1_MIRICETINA=aov(MIRICETINA~ORGANOS,data = datos)
summary(modelo1_MIRICETINA)
#No Hay diferencias significativas entre organos para el metabolito MIRICETINA

#Como aun no hay significancia, estas pruebas no son necesarias, pero queda el codigo por si cambia
#Post-hoc procedimiento:
#Para conocer que grupos de edad son los que difieren, se deben realizar contrastes dos a dos
#Procedimiento posthoc contrasta diferencias de todos los grupos e identifica aquellas diferencias estadisticamente significativas
pairwise.t.test( datos$MIRICETINA, 
                 datos$ORGANOS, 
                 p.adj = "bonferroni")

#Interpretación: 
#No Existen diferencias significativas entre organos


#Prueba de medias TUKEY #Es la misma que el procedimiento POSTHOC pero mas sencillo de entender
#Como la prueba ANOVA es significativa, podemos calcular Tukey HSD (Diferencias significativas honestas de Tukey, función R: TukeyHSD()) 
#para realizar múltiples comparaciones por pares entre las medias de los grupos.
#Las diferencias seran significativas cuando el pvalue sea menor a 0.05
TukeyHSD(modelo1_MIRICETINA)

#Eso significa
#diff: difference between means of the two groups
#lwr, upr: the lower and the upper end point of the confidence interval at 95% (default)
#p adj: p-value after adjustment for the multiple comparisons.

#REVISIÓN DEL MODELO ANOVA
jpeg(filename = "Diagnostico_Anova_1via_MIRICETINA.jpeg",width = 190,height = 130,units = "mm",res = 300) #No cambiar 190 
par(mfrow=c(2,2))
plot(modelo1_MIRICETINA)
dev.off()

#Cumplimiento de supuestos
#Homogeneidad de varianza
leveneTest(MIRICETINA ~ ORGANOS, data = datos) #Veamos como cambia esta una vez que tienes todos tus datos - de momento se cumple el supuesto
shapiro.test(residuals(modelo1_MIRICETINA)) #Veamos como cambia esta una vez que tienes todos tus datos


#METABOLITO: ISORHAMNETINA

#Anova 1 via:
modelo1_ISORHAMNETINA=aov(ISORHAMNETINA~ORGANOS,data = datos)
summary(modelo1_ISORHAMNETINA)
#No Hay diferencias significativas entre organos para el metabolito ISORHAMNETINA


#Post-hoc procedimiento:
#Para conocer que grupos de edad son los que difieren, se deben realizar contrastes dos a dos
#Procedimiento posthoc contrasta diferencias de todos los grupos e identifica aquellas diferencias estadisticamente significativas
pairwise.t.test( datos$ISORHAMNETINA, 
                 datos$ORGANOS, 
                 p.adj = "bonferroni")

#Interpretación: 
#No Existen diferencias significativas (p-valor < 0.05) entre organos


#Prueba de medias TUKEY #Es la misma que el procedimiento POSTHOC pero mas sencillo de entender
#Como la prueba ANOVA es significativa, podemos calcular Tukey HSD (Diferencias significativas honestas de Tukey, función R: TukeyHSD()) 
#para realizar múltiples comparaciones por pares entre las medias de los grupos.
#Las diferencias seran significativas cuando el pvalue sea menor a 0.05
TukeyHSD(modelo1_ISORHAMNETINA)

#Eso significa
#diff: difference between means of the two groups
#lwr, upr: the lower and the upper end point of the confidence interval at 95% (default)
#p adj: p-value after adjustment for the multiple comparisons.

#REVISIÓN DEL MODELO ANOVA
jpeg(filename = "Diagnostico_Anova_1via_ISORHAMNETINA.jpeg",width = 190,height = 130,units = "mm",res = 300) #No cambiar 190 
par(mfrow=c(2,2))
plot(modelo1_ISORHAMNETINA)
dev.off()

#Cumplimiento de supuestos
#Homogeneidad de varianza
leveneTest(ISORHAMNETINA ~ ORGANOS, data = datos) #Veamos como cambia esta una vez que tienes todos tus datos - de momento se cumple el supuesto
shapiro.test(residuals(modelo1_ISORHAMNETINA)) #Veamos como cambia esta una vez que tienes todos tus datos


#METABOLITO: HESPERIDINA

#Anova 1 via:
modelo1_HESPERIDINA=aov(HESPERIDINA~ORGANOS,data = datos)
summary(modelo1_HESPERIDINA)
#Hay diferencias significativas entre organos para el metabolito HESPERIDINA


#Post-hoc procedimiento:
#Para conocer que grupos de edad son los que difieren, se deben realizar contrastes dos a dos
#Procedimiento posthoc contrasta diferencias de todos los grupos e identifica aquellas diferencias estadisticamente significativas
pairwise.t.test( datos$HESPERIDINA, 
                 datos$ORGANOS, 
                 p.adj = "bonferroni")

#Interpretación: 
#Existen diferencias significativas (p-valor < 0.05) entre: 
#Suero y Orina (2.4e-10)

#Prueba de medias TUKEY #Es la misma que el procedimiento POSTHOC pero mas sencillo de entender
#Como la prueba ANOVA es significativa, podemos calcular Tukey HSD (Diferencias significativas honestas de Tukey, función R: TukeyHSD()) 
#para realizar múltiples comparaciones por pares entre las medias de los grupos.
#Las diferencias seran significativas cuando el pvalue sea menor a 0.05
TukeyHSD(modelo1_HESPERIDINA)

#Eso significa
#diff: difference between means of the two groups
#lwr, upr: the lower and the upper end point of the confidence interval at 95% (default)
#p adj: p-value after adjustment for the multiple comparisons.

#REVISIÓN DEL MODELO ANOVA
jpeg(filename = "Diagnostico_Anova_1via_HESPERIDINA.jpeg",width = 190,height = 130,units = "mm",res = 300) #No cambiar 190 
par(mfrow=c(2,2))
plot(modelo1_HESPERIDINA)
dev.off()

#Cumplimiento de supuestos
#Homogeneidad de varianza
leveneTest(HESPERIDINA ~ ORGANOS, data = datos) #Veamos como cambia esta una vez que tienes todos tus datos - de momento se cumple el supuesto
shapiro.test(residuals(modelo1_HESPERIDINA)) #Veamos como cambia esta una vez que tienes todos tus datos



#METABOLITO: KAEMFEROL

#Anova 1 via:
modelo1_KAEMFEROL=aov(KAEMFEROL~ORGANOS,data = datos)
summary(modelo1_KAEMFEROL)
#No Hay diferencias significativas entre organos para el metabolito KAEMFEROL


#Post-hoc procedimiento:
#Para conocer que grupos de edad son los que difieren, se deben realizar contrastes dos a dos
#Procedimiento posthoc contrasta diferencias de todos los grupos e identifica aquellas diferencias estadisticamente significativas
pairwise.t.test( datos$KAEMFEROL, 
                 datos$ORGANOS, 
                 p.adj = "bonferroni")

#Interpretación: 
#No Existen diferencias significativas (p-valor < 0.05) entre organos

#Prueba de medias TUKEY #Es la misma que el procedimiento POSTHOC pero mas sencillo de entender
#Como la prueba ANOVA es significativa, podemos calcular Tukey HSD (Diferencias significativas honestas de Tukey, función R: TukeyHSD()) 
#para realizar múltiples comparaciones por pares entre las medias de los grupos.
#Las diferencias seran significativas cuando el pvalue sea menor a 0.05
TukeyHSD(modelo1_KAEMFEROL)

#Eso significa
#diff: difference between means of the two groups
#lwr, upr: the lower and the upper end point of the confidence interval at 95% (default)
#p adj: p-value after adjustment for the multiple comparisons.

#REVISIÓN DEL MODELO ANOVA
jpeg(filename = "Diagnostico_Anova_1via_KAEMFEROL.jpeg",width = 190,height = 130,units = "mm",res = 300) #No cambiar 190 
par(mfrow=c(2,2))
plot(modelo1_KAEMFEROL)
dev.off()

#Cumplimiento de supuestos
#Homogeneidad de varianza
leveneTest(KAEMFEROL ~ ORGANOS, data = datos) #Veamos como cambia esta una vez que tienes todos tus datos - de momento se cumple el supuesto
shapiro.test(residuals(modelo1_KAEMFEROL)) #Veamos como cambia esta una vez que tienes todos tus datos



#METABOLITO: QUERCETINA

#Anova 1 via:
modelo1_QUERCETINA=aov(QUERCETINA~ORGANOS,data = datos)
summary(modelo1_QUERCETINA)
#Hay diferencias significativas entre organos para el metabolito QUERCETINA


#Post-hoc procedimiento:
#Para conocer que grupos de edad son los que difieren, se deben realizar contrastes dos a dos
#Procedimiento posthoc contrasta diferencias de todos los grupos e identifica aquellas diferencias estadisticamente significativas
pairwise.t.test( datos$QUERCETINA, 
                 datos$ORGANOS, 
                 p.adj = "bonferroni")

#Interpretación: 
#Faltan datos

#Prueba de medias TUKEY #Es la misma que el procedimiento POSTHOC pero mas sencillo de entender
#Como la prueba ANOVA es significativa, podemos calcular Tukey HSD (Diferencias significativas honestas de Tukey, función R: TukeyHSD()) 
#para realizar múltiples comparaciones por pares entre las medias de los grupos.
#Las diferencias seran significativas cuando el pvalue sea menor a 0.05
TukeyHSD(modelo1_QUERCETINA)

#Eso significa
#diff: difference between means of the two groups
#lwr, upr: the lower and the upper end point of the confidence interval at 95% (default)
#p adj: p-value after adjustment for the multiple comparisons.

#REVISIÓN DEL MODELO ANOVA
jpeg(filename = "Diagnostico_Anova_1via_QUERCETINA.jpeg",width = 190,height = 130,units = "mm",res = 300) #No cambiar 190 
par(mfrow=c(2,2))
plot(modelo1_QUERCETINA)
dev.off()

#Cumplimiento de supuestos
#Homogeneidad de varianza
leveneTest(QUERCETINA ~ ORGANOS, data = datos) #Veamos como cambia esta una vez que tienes todos tus datos - de momento se cumple el supuesto
shapiro.test(residuals(modelo1_QUERCETINA)) #Veamos como cambia esta una vez que tienes todos tus datos


#METABOLITO: CATEQUINA

#Anova 1 via:
modelo1_CATEQUINA=aov(CATEQUINA~ORGANOS,data = datos)
summary(modelo1_CATEQUINA)
#Hay diferencias significativas entre organos para el metabolito CATEQUINA


#Post-hoc procedimiento:
#Para conocer que grupos de edad son los que difieren, se deben realizar contrastes dos a dos
#Procedimiento posthoc contrasta diferencias de todos los grupos e identifica aquellas diferencias estadisticamente significativas
pairwise.t.test( datos$CATEQUINA, 
                 datos$ORGANOS, 
                 p.adj = "bonferroni")

#Interpretación: 
#Faltan datos

#Prueba de medias TUKEY #Es la misma que el procedimiento POSTHOC pero mas sencillo de entender
#Como la prueba ANOVA es significativa, podemos calcular Tukey HSD (Diferencias significativas honestas de Tukey, función R: TukeyHSD()) 
#para realizar múltiples comparaciones por pares entre las medias de los grupos.
#Las diferencias seran significativas cuando el pvalue sea menor a 0.05
TukeyHSD(modelo1_CATEQUINA)

#Eso significa
#diff: difference between means of the two groups
#lwr, upr: the lower and the upper end point of the confidence interval at 95% (default)
#p adj: p-value after adjustment for the multiple comparisons.

#REVISIÓN DEL MODELO ANOVA
jpeg(filename = "Diagnostico_Anova_1via_CATEQUINA.jpeg",width = 190,height = 130,units = "mm",res = 300) #No cambiar 190 
par(mfrow=c(2,2))
plot(modelo1_CATEQUINA)
dev.off()

#Cumplimiento de supuestos
#Homogeneidad de varianza
leveneTest(CATEQUINA ~ ORGANOS, data = datos) #Veamos como cambia esta una vez que tienes todos tus datos - de momento se cumple el supuesto
shapiro.test(residuals(modelo1_CATEQUINA)) #Veamos como cambia esta una vez que tienes todos tus datos



#METABOLITO: FLORITZINA

#Anova 1 via:
modelo1_FLORITZINA=aov(FLORITZINA~ORGANOS,data = datos)
summary(modelo1_FLORITZINA)
#No Hay diferencias significativas entre organos para el metabolito FLORITZINA


#Post-hoc procedimiento:
#Para conocer que grupos de edad son los que difieren, se deben realizar contrastes dos a dos
#Procedimiento posthoc contrasta diferencias de todos los grupos e identifica aquellas diferencias estadisticamente significativas
pairwise.t.test( datos$FLORITZINA, 
                 datos$ORGANOS, 
                 p.adj = "bonferroni")

#Interpretación: 
#Faltan datos

#Prueba de medias TUKEY #Es la misma que el procedimiento POSTHOC pero mas sencillo de entender
#Como la prueba ANOVA es significativa, podemos calcular Tukey HSD (Diferencias significativas honestas de Tukey, función R: TukeyHSD()) 
#para realizar múltiples comparaciones por pares entre las medias de los grupos.
#Las diferencias seran significativas cuando el pvalue sea menor a 0.05
TukeyHSD(modelo1_FLORITZINA)

#Eso significa
#diff: difference between means of the two groups
#lwr, upr: the lower and the upper end point of the confidence interval at 95% (default)
#p adj: p-value after adjustment for the multiple comparisons.

#REVISIÓN DEL MODELO ANOVA
jpeg(filename = "Diagnostico_Anova_1via_FLORITZINA.jpeg",width = 190,height = 130,units = "mm",res = 300) #No cambiar 190 
par(mfrow=c(2,2))
plot(modelo1_FLORITZINA)
dev.off()

#Cumplimiento de supuestos
#Homogeneidad de varianza
leveneTest(FLORITZINA ~ ORGANOS, data = datos) #Veamos como cambia esta una vez que tienes todos tus datos - de momento se cumple el supuesto
shapiro.test(residuals(modelo1_FLORITZINA)) #Veamos como cambia esta una vez que tienes todos tus datos


#METABOLITO: CUCURBITACINA.D

#Anova 1 via:
modelo1_CUCURBITACINA.D=aov(CUCURBITACINA.D~ORGANOS,data = datos)
summary(modelo1_CUCURBITACINA.D)
#No Hay diferencias significativas entre organos para el metabolito CUCURBITACINA.D


#Post-hoc procedimiento:
#Para conocer que grupos de edad son los que difieren, se deben realizar contrastes dos a dos
#Procedimiento posthoc contrasta diferencias de todos los grupos e identifica aquellas diferencias estadisticamente significativas
pairwise.t.test( datos$CUCURBITACINA.D, 
                 datos$ORGANOS, 
                 p.adj = "bonferroni")

#Interpretación: 
#Faltan datos

#Prueba de medias TUKEY #Es la misma que el procedimiento POSTHOC pero mas sencillo de entender
#Como la prueba ANOVA es significativa, podemos calcular Tukey HSD (Diferencias significativas honestas de Tukey, función R: TukeyHSD()) 
#para realizar múltiples comparaciones por pares entre las medias de los grupos.
#Las diferencias seran significativas cuando el pvalue sea menor a 0.05
TukeyHSD(modelo1_CUCURBITACINA.D)

#Eso significa
#diff: difference between means of the two groups
#lwr, upr: the lower and the upper end point of the confidence interval at 95% (default)
#p adj: p-value after adjustment for the multiple comparisons.

#REVISIÓN DEL MODELO ANOVA
jpeg(filename = "Diagnostico_Anova_1via_CUCURBITACINA.D.jpeg",width = 190,height = 130,units = "mm",res = 300) #No cambiar 190 
par(mfrow=c(2,2))
plot(modelo1_CUCURBITACINA.D)
dev.off()

#Cumplimiento de supuestos
#Homogeneidad de varianza
leveneTest(CUCURBITACINA.D ~ ORGANOS, data = datos) #Veamos como cambia esta una vez que tienes todos tus datos - de momento se cumple el supuesto
shapiro.test(residuals(modelo1_CUCURBITACINA.D)) #Veamos como cambia esta una vez que tienes todos tus datos


#METABOLITO: CUCURBITACINA.I

#Anova 1 via:
modelo1_CUCURBITACINA.I=aov(CUCURBITACINA.I~ORGANOS,data = datos)
summary(modelo1_CUCURBITACINA.I)
#No Hay diferencias significativas entre organos para el metabolito CUCURBITACINA.I


#Post-hoc procedimiento:
#Para conocer que grupos de edad son los que difieren, se deben realizar contrastes dos a dos
#Procedimiento posthoc contrasta diferencias de todos los grupos e identifica aquellas diferencias estadisticamente significativas
pairwise.t.test( datos$CUCURBITACINA.I, 
                 datos$ORGANOS, 
                 p.adj = "bonferroni")

#Interpretación: 
#Faltan datos

#Prueba de medias TUKEY #Es la misma que el procedimiento POSTHOC pero mas sencillo de entender
#Como la prueba ANOVA es significativa, podemos calcular Tukey HSD (Diferencias significativas honestas de Tukey, función R: TukeyHSD()) 
#para realizar múltiples comparaciones por pares entre las medias de los grupos.
#Las diferencias seran significativas cuando el pvalue sea menor a 0.05
TukeyHSD(modelo1_CUCURBITACINA.I)

#Eso significa
#diff: difference between means of the two groups
#lwr, upr: the lower and the upper end point of the confidence interval at 95% (default)
#p adj: p-value after adjustment for the multiple comparisons.

#REVISIÓN DEL MODELO ANOVA
jpeg(filename = "Diagnostico_Anova_1via_CUCURBITACINA.I.jpeg",width = 190,height = 130,units = "mm",res = 300) #No cambiar 190 
par(mfrow=c(2,2))
plot(modelo1_CUCURBITACINA.I)
dev.off()

#Cumplimiento de supuestos
#Homogeneidad de varianza
leveneTest(CUCURBITACINA.I ~ ORGANOS, data = datos) #Veamos como cambia esta una vez que tienes todos tus datos - de momento se cumple el supuesto
shapiro.test(residuals(modelo1_CUCURBITACINA.I)) #Veamos como cambia esta una vez que tienes todos tus datos


#METABOLITO: CUCURBITACINA.IIA

#Anova 1 via:
modelo1_CUCURBITACINA.IIA=aov(CUCURBITACINA.IIA~ORGANOS,data = datos)
summary(modelo1_CUCURBITACINA.IIA)
#No Hay diferencias significativas entre organos para el metabolito CUCURBITACINA.IIA


#Post-hoc procedimiento:
#Para conocer que grupos de edad son los que difieren, se deben realizar contrastes dos a dos
#Procedimiento posthoc contrasta diferencias de todos los grupos e identifica aquellas diferencias estadisticamente significativas
pairwise.t.test( datos$CUCURBITACINA.IIA, 
                 datos$ORGANOS, 
                 p.adj = "bonferroni")

#Interpretación: 
#Faltan datos

#Prueba de medias TUKEY #Es la misma que el procedimiento POSTHOC pero mas sencillo de entender
#Como la prueba ANOVA es significativa, podemos calcular Tukey HSD (Diferencias significativas honestas de Tukey, función R: TukeyHSD()) 
#para realizar múltiples comparaciones por pares entre las medias de los grupos.
#Las diferencias seran significativas cuando el pvalue sea menor a 0.05
TukeyHSD(modelo1_CUCURBITACINA.IIA)

#Eso significa
#diff: difference between means of the two groups
#lwr, upr: the lower and the upper end point of the confidence interval at 95% (default)
#p adj: p-value after adjustment for the multiple comparisons.

#REVISIÓN DEL MODELO ANOVA
jpeg(filename = "Diagnostico_Anova_1via_CUCURBITACINA.IIA.jpeg",width = 190,height = 130,units = "mm",res = 300) #No cambiar 190 
par(mfrow=c(2,2))
plot(modelo1_CUCURBITACINA.IIA)
dev.off()

#Cumplimiento de supuestos
#Homogeneidad de varianza
leveneTest(CUCURBITACINA.IIA ~ ORGANOS, data = datos) #Veamos como cambia esta una vez que tienes todos tus datos - de momento se cumple el supuesto
shapiro.test(residuals(modelo1_CUCURBITACINA.IIA)) #Veamos como cambia esta una vez que tienes todos tus datos


#METABOLITO: CUCURBITACINA.B

#Anova 1 via:
modelo1_CUCURBITACINA.B=aov(CUCURBITACINA.B~ORGANOS,data = datos)
summary(modelo1_CUCURBITACINA.B)
#No Hay diferencias significativas entre organos para el metabolito CUCURBITACINA.B


#Post-hoc procedimiento:
#Para conocer que grupos de edad son los que difieren, se deben realizar contrastes dos a dos
#Procedimiento posthoc contrasta diferencias de todos los grupos e identifica aquellas diferencias estadisticamente significativas
pairwise.t.test( datos$CUCURBITACINA.B, 
                 datos$ORGANOS, 
                 p.adj = "bonferroni")

#Interpretación: 
#Faltan datos

#Prueba de medias TUKEY #Es la misma que el procedimiento POSTHOC pero mas sencillo de entender
#Como la prueba ANOVA es significativa, podemos calcular Tukey HSD (Diferencias significativas honestas de Tukey, función R: TukeyHSD()) 
#para realizar múltiples comparaciones por pares entre las medias de los grupos.
#Las diferencias seran significativas cuando el pvalue sea menor a 0.05
TukeyHSD(modelo1_CUCURBITACINA.B)

#Eso significa
#diff: difference between means of the two groups
#lwr, upr: the lower and the upper end point of the confidence interval at 95% (default)
#p adj: p-value after adjustment for the multiple comparisons.

#REVISIÓN DEL MODELO ANOVA
jpeg(filename = "Diagnostico_Anova_1via_CUCURBITACINA.B.jpeg",width = 190,height = 130,units = "mm",res = 300) #No cambiar 190 
par(mfrow=c(2,2))
plot(modelo1_CUCURBITACINA.B)
dev.off()

#Cumplimiento de supuestos
#Homogeneidad de varianza
leveneTest(CUCURBITACINA.B ~ ORGANOS, data = datos) #Veamos como cambia esta una vez que tienes todos tus datos - de momento se cumple el supuesto
shapiro.test(residuals(modelo1_CUCURBITACINA.B)) #Veamos como cambia esta una vez que tienes todos tus datos


#METABOLITO: CUCURBITACINA.E

#Anova 1 via:
modelo1_CUCURBITACINA.E=aov(CUCURBITACINA.E~ORGANOS,data = datos)
summary(modelo1_CUCURBITACINA.E)
#No Hay diferencias significativas entre organos para el metabolito CUCURBITACINA.E


#Post-hoc procedimiento:
#Para conocer que grupos de edad son los que difieren, se deben realizar contrastes dos a dos
#Procedimiento posthoc contrasta diferencias de todos los grupos e identifica aquellas diferencias estadisticamente significativas
pairwise.t.test( datos$CUCURBITACINA.E, 
                 datos$ORGANOS, 
                 p.adj = "bonferroni")

#Interpretación: 
#Faltan datos

#Prueba de medias TUKEY #Es la misma que el procedimiento POSTHOC pero mas sencillo de entender
#Como la prueba ANOVA es significativa, podemos calcular Tukey HSD (Diferencias significativas honestas de Tukey, función R: TukeyHSD()) 
#para realizar múltiples comparaciones por pares entre las medias de los grupos.
#Las diferencias seran significativas cuando el pvalue sea menor a 0.05
TukeyHSD(modelo1_CUCURBITACINA.E)

#Eso significa
#diff: difference between means of the two groups
#lwr, upr: the lower and the upper end point of the confidence interval at 95% (default)
#p adj: p-value after adjustment for the multiple comparisons.

#REVISIÓN DEL MODELO ANOVA
jpeg(filename = "Diagnostico_Anova_1via_CUCURBITACINA.E.jpeg",width = 190,height = 130,units = "mm",res = 300) #No cambiar 190 
par(mfrow=c(2,2))
plot(modelo1_CUCURBITACINA.E)
dev.off()

#Cumplimiento de supuestos
#Homogeneidad de varianza
leveneTest(CUCURBITACINA.E ~ ORGANOS, data = datos) #Veamos como cambia esta una vez que tienes todos tus datos - de momento se cumple el supuesto
shapiro.test(residuals(modelo1_CUCURBITACINA.E)) #Veamos como cambia esta una vez que tienes todos tus datos

#------------------------------------------------------
# 6.2.  KRUSKAL WALLIS
#------------------------------------------------------
#Como no hay normalidad aun se prueba un analisis no parametrico
#una alternativa no paramétrica al ANOVA unidireccional es la prueba de suma de rangos de Kruskal-Wallis, que se puede utilizar cuando no se cumplen los supuestos del ANNOVA.
#El test encuentra significancia en la diferencia de al menos dos grupos. 


#DIFERENCIAS ENTRE ORGANOS

kruskal.test(RUTINA~ORGANOS,data = datos)
kruskal.test(NARINGENINA~ORGANOS,data = datos)
kruskal.test(FLORETINA~ORGANOS,data = datos)
kruskal.test(MORINA~ORGANOS,data = datos)
kruskal.test(APIGENINA~ORGANOS,data = datos)
kruskal.test(MIRICETINA~ORGANOS,data = datos)
kruskal.test(ISORHAMNETINA~ORGANOS,data = datos)
kruskal.test(HESPERIDINA~ORGANOS,data = datos)
kruskal.test(KAEMFEROL~ORGANOS,data = datos)
kruskal.test(QUERCETINA~ORGANOS,data = datos)
kruskal.test(CATEQUINA~ORGANOS,data = datos)
kruskal.test(FLORITZINA~ORGANOS,data = datos)
kruskal.test(CUCURBITACINA.D~ORGANOS,data = datos)
kruskal.test(CUCURBITACINA.I~ORGANOS,data = datos)
kruskal.test(CUCURBITACINA.IIA~ORGANOS,data = datos)
kruskal.test(CUCURBITACINA.B~ORGANOS,data = datos)
kruskal.test(CUCURBITACINA.E~ORGANOS,data = datos)


#------------------------------------------------------
# 6.3 ANOVA DE DOS VIAS ORGANOS + TRATAMIENTOS + TIEMPO
#------------------------------------------------------

#METABOLITO: RUTINA
names(datos)
modelo2_RUTINA <- aov(RUTINA~ORGANOS+TRATAMIENTO+TIEMPO,data = datos)
summary(modelo2_RUTINA)
#CONCLUSION
#Hay diferencias significativas entre organos para este metabolito
#No hay diferencias significativas entre tratamientos ni tiempo
modelo2_NARINGENINA <- aov(NARINGENINA~ORGANOS+TRATAMIENTO+TIEMPO,data = datos)
summary(modelo2_NARINGENINA) 
#CONCLUSION
#Hay diferencias significativas entre organos para este metabolito
#No hay diferencias significativas entre tratamientos ni tiempo

modelo2_FLORETINA <- aov(FLORETINA~ORGANOS+TRATAMIENTO+TIEMPO,data = datos)
summary(modelo2_FLORETINA) 
#CONCLUSION
#Hay diferencias significativas entre organos para este metabolito
#No hay diferencias significativas entre tratamientos ni tiempo

modelo2_MORINA <- aov(MORINA~ORGANOS+TRATAMIENTO+TIEMPO,data = datos)
summary(modelo2_MORINA) 
#CONCLUSION
#No Hay diferencias significativas entre organos para este metabolito
#No hay diferencias significativas entre tratamientos ni tiempo


modelo2_APIGENINA <- aov(APIGENINA~ORGANOS+TRATAMIENTO+TIEMPO,data = datos)
summary(modelo2_APIGENINA) 
#CONCLUSION
#Hay diferencias significativas entre organos para este metabolito
#No hay diferencias significativas entre tratamientos ni tiempo


modelo2_MIRICETINA <- aov(MIRICETINA~ORGANOS+TRATAMIENTO+TIEMPO,data = datos)
summary(modelo2_MIRICETINA) 
#CONCLUSION
#No Hay diferencias significativas entre organos para este metabolito
#No hay diferencias significativas entre tratamientos ni tiempo


modelo2_ISORHAMNETINA<- aov(ISORHAMNETINA~ORGANOS+TRATAMIENTO+TIEMPO,data = datos)
summary(modelo2_ISORHAMNETINA) 
#CONCLUSION
#No Hay diferencias significativas entre organos para este metabolito
#No hay diferencias significativas entre tratamientos ni tiempo

modelo2_HESPERIDINA<- aov(HESPERIDINA~ORGANOS+TRATAMIENTO+TIEMPO,data = datos)
summary(modelo2_HESPERIDINA) 
#CONCLUSION
#Hay diferencias significativas entre organos y tratamientos para este metabolito
#No hay diferencias significativas entre  tiempo

modelo2_KAEMFEROL <- aov(KAEMFEROL~ORGANOS+TRATAMIENTO+TIEMPO,data = datos)
summary(modelo2_KAEMFEROL) 
#CONCLUSION
#No Hay diferencias significativas entre organos para este metabolito
#No hay diferencias significativas entre tratamientos ni tiempo


modelo2_QUERCETINA <- aov(QUERCETINA~ORGANOS+TRATAMIENTO+TIEMPO,data = datos)
summary(modelo2_QUERCETINA) 
#CONCLUSION
#Hay diferencias significativas entre organos para este metabolito
#No hay diferencias significativas entre tratamientos ni tiempo

modelo2_CATEQUINA <- aov(CATEQUINA~ORGANOS+TRATAMIENTO+TIEMPO,data = datos)
summary(modelo2_CATEQUINA) 
#CONCLUSION
#Hay diferencias significativas entre organos y tiempos para este metabolito
#No hay diferencias significativas entre tratamientos 

modelo2_FLORITZINA <- aov(FLORITZINA~ORGANOS+TRATAMIENTO+TIEMPO,data = datos)
summary(modelo2_FLORITZINA) 
#CONCLUSION
#Hay diferencias significativas entre tiempos para este metabolito
#No hay diferencias significativas entre tratamientos ni organos


modelo2_CUCURBITACINA.D<- aov(CUCURBITACINA.D~ORGANOS+TRATAMIENTO+TIEMPO,data = datos)
summary(modelo2_CUCURBITACINA.D) 
#CONCLUSION
#Hay diferencias significativas entre tiempo para este metabolito
#No hay diferencias significativas entre tratamientos ni organos


modelo2_CUCURBITACINA.I <- aov(CUCURBITACINA.I~ORGANOS+TRATAMIENTO+TIEMPO,data = datos)
summary(modelo2_CUCURBITACINA.I) 
#CONCLUSION
#No Hay diferencias significativas entre organos para este metabolito
#No hay diferencias significativas entre tratamientos ni tiempo

modelo2_CUCURBITACINA.IIA <- aov(CUCURBITACINA.IIA~ORGANOS+TRATAMIENTO+TIEMPO,data = datos)
summary(modelo2_CUCURBITACINA.IIA) 
#CONCLUSION
#NO Hay diferencias significativas entre organos para este metabolito
#No hay diferencias significativas entre tratamientos ni tiempo

modelo2_CUCURBITACINA.B <- aov(CUCURBITACINA.B~ORGANOS+TRATAMIENTO+TIEMPO,data = datos)
summary(modelo2_CUCURBITACINA.B) 
#CONCLUSION
#Hay diferencias significativas entre tiempo para este metabolito
#No hay diferencias significativas entre tratamientos ni organos


modelo2_CUCURBITACINA.E <- aov(CUCURBITACINA.E~ORGANOS+TRATAMIENTO+TIEMPO,data = datos)
summary(modelo2_CUCURBITACINA.E) 
#CONCLUSION
#Hay diferencias significativas entre tiempo para este metabolito
#No hay diferencias significativas entre tratamientos ni organos

#------------------------------------------------------
# 6.4 EFECTOS DE LOS TRATAMIENTOS
#------------------------------------------------------

model.tables(modelo2_APIGENINA)
model.tables(modelo2_CATEQUINA)
model.tables(modelo2_FLORETINA)
model.tables(modelo2_FLORITZINA)
model.tables(modelo2_HESPERIDINA)
model.tables(modelo2_ISORHAMNETINA)
model.tables(modelo2_KAEMFEROL)
model.tables(modelo2_MIRICETINA)
model.tables(modelo2_MORINA)
model.tables(modelo2_NARINGENINA)
model.tables(modelo2_QUERCETINA)
model.tables(modelo2_RUTINA)
model.tables(modelo2_CUCURBITACINA.B)
model.tables(modelo2_CUCURBITACINA.D)
model.tables(modelo2_CUCURBITACINA.E)
model.tables(modelo2_CUCURBITACINA.I)
model.tables(modelo2_CUCURBITACINA.IIA)

###Recomendación: MODELO LINEAL MIXTO
#UNA VEZ QUE TENGAS TODOS TUS DATOS, TE RECOMIENDO QUE CONSIDERES UN MODELO DE EFECTOS MIXTOS, PARECE Q TUS DATOS SE AJUSTARIAN BIEN
