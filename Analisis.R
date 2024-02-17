
#Se cargan librerías y las bases de datos de las calificaciones 
calificaciones_prepa <- read.csv("E:/Descargas/calificaciones_prepa.csv")
install.packages("tidyverse")
install.packages("rpart")
library(rpart)
library(tidyverse)
library(dplyr)
library(ggplot)
#se buscan valores nulos dentro de data frame 
any(is.nan(calificaciones_prepa))
mean(is.na(calificaciones_prepa))

#se obtiene de la base de datos solo la calificacion de matematicas 1
Mate_1<- select(calificaciones_prepa,Desc_Materia, sexo, Calif_Final) %>% filter(Desc_Materia=="MATEMATICAS_I")

#Revisando los datos mediante tablas de frecuencia y graficas de frecuencia
Mate_1 %>% count(sexo) %>% group_by(sexo)
#se realiza grafica de barras de mateáticas 1 por sexo
ggplot(calificaciones_prepa, aes(x=sexo))+geom_bar(fill="purple")

#se obtiene media, desviacion estandar y mediana de matemáticas 1
Media <-mean(Mate_1$Calif_Final)
Desviacion <-sd(Mate_1$Calif_Final)
Mediana <-median(Mate_1$Calif_Final) 
print(paste("Promedio Calificacion Matemáticas I",Media))
print(paste("Desviación estandar Matemáticas I",Desviacion))
print(paste("Mediana de Matemáticas I",Mediana))

#obtenemos cuartiles de Matemáticas 1
quantile(x=Mate_1$Calif_Final,probs = c(0.25, 0.50, 0.75), type = 0)

#boxplot de matemáticas 1
ggplot(data = Mate_1, mapping = aes(y=Calif_Final))+ geom_boxplot(outlier.colour = "red") + labs((title="Calificaciones de Matemáticas 1"))
           
                                                                                                  