library (tidyverse)
getwd ()
setwd ("C:/a_a_Ana_Karen/Cursos_Otoño_2022/ANOVA_M")
datos <- read.csv("completa.csv", header=TRUE, sep=",")
library (tidyverse)
datos1<-datos %>% 
group_by(edad) %>% 
summarise(media=mean(flujo),
          des_estan=sd(flujo),
          minimo=min(flujo)) %>% 
  ungroup()
datos1
class (datos1$flujo)
colnames(datos)
aov(media~edad, data=datos1)
resultado_aov<-aov(media~edad, data=datos1)
summary(resultado_aov)
resultado_aov1<-aov(flujo~edad, data=datos)
summary(resultado_aov1)
