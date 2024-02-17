
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

#RHistograma de matemáticas 1
hist(Mate_1$Calif_Final, main = "Distribucion de calificacione Matemáticas 1",
     xlab="Calificacion",ylab = "Frecuencia", col = "red", border = "white")

#densidad de probabilidad para Matemáticas 1
x_m <-seq(0,20,length.out=100)
y_m <-dnorm(x_m, mean = Media, sd=Desviacion)
plot(x_m,y_m,type="l",lwd=2, col="red", 
     main = "Distribucion normal de Matemáticas 1",
     xlab="Calificaciones", ylab = "Densidad de probabiliodad")

#prueba de normalidad para la distribucion de Matemáticas 1
## Prueba de Normalidad Calificaciones Matemáticas: Shapiro-Wilk

PNormalidadSW <- shapiro.test(Mate_1$Calif_Final)
print(PNormalidadSW) #Un p-value menor a 0.05 indica que el conjunto de datos tiende a una distribución normal.

#Gráfico de la distribución
plotn <- function(x,main="Histograma de frecuencias \ny distribución normal",
                  xlab="Calificaciones G3",ylab="Densidad") {
  min <- min(Mate_1$Calif_Final)
  max <- max(Mate_1$Calif_Final)
  media <- mean(Mate_1$Calif_Final)
  dt <- sd(Mate_1$Calif_Final)
  hist(Mate_1$Calif_Final,freq=F,main=main,xlab=xlab,ylab=ylab)
  curve(dnorm(x,media,dt), min, max,add = T,col="red")
}

plotn(x,main="Distribución de Calificaciones Matemáticas") #Grafico de valores

#boxplot de matemáticas 1 por sexo
ggplot(Mate_1,aes(x=sexo, y=Calif_Final))+geom_boxplot()

#prueba de hipotesis
#ANOVA Matemáticas (Diferencia de Medias entre hombres y mujeres).

########                     ANOVA            ########################################################
# Se realiza el análisis de varianza (ANOVA)
# utilizando la función aov(), con la calificación final  como variable dependiente y el
# sexo como variable independiente.

modelo_anova <- aov(Calif_Final ~ sexo, data = Mate_1)

# Se otienen los resultados de ANOVA mediante la función summary
summary(modelo_anova)


#Prueba t de Stydent
#### Prueba de Hipótesis para Matemáticas ###

# Realizar la prueba t de student para comparar la media de las calificaciones finales entre hombres y mujeres para Matemáticas.

#Ho: La diferencia de medias entre hombres y mujeres es igual a cero.
#Ha: La diferencia de medias entre hombres y mujeres es diferente de cero.

prueba_t <- t.test(Calif_Final ~ sexo, data = Mate_1)

# Mostrar los resultados de la prueba t
prueba_t
