
#Se cargan librerías y las bases de datos de las calificaciones 
calificaciones_prepa <- read.csv("E:/Descargas/calificaciones_prepa.csv")
install.packages("tidyverse")
install.packages("rpart")
install.packages("patchwork")
library(rpart)
library(tidyverse)
library(dplyr)
library(patchwork)


#se obtiene de la base de datos solo la calificacion de matematicas 1
Mate_1<- select(calificaciones_prepa,Desc_Materia, sexo, Calif_Final) %>% filter(Desc_Materia=="MATEMATICAS_I")
Lec <- select(calificaciones_prepa,Desc_Materia, sexo, Calif_Final) %>% filter(Desc_Materia=="TALL.DE_LECTURA_Y_REDACCION_I")

#se obtiene calificacion solo de masculino (M) de Matematicas 1
Mate_m<-filter(Mate_1, sexo=="M")
Lec_m<-filter(Lec, sexo=="M")
#se obtiene calificacion solo de femenino (F) de Matematicas 1
Mate_f<-filter(Mate_1, sexo=="F")
Lec_f<-filter(Lec, sexo=="F")

#Revisando los datos mediante tablas de frecuencia y graficas de frecuencia
Mate_1 %>% count(sexo) %>% group_by(sexo)
Lec %>% count(sexo) %>% group_by(sexo)
Mate_m %>% count(Calif_Final) %>% group_by(Calif_Final)
Mate_f %>% count(Calif_Final) %>% group_by(Calif_Final)

Lec_m %>% count(Calif_Final) %>% group_by(Calif_Final)
Lec_f %>% count(Calif_Final) %>% group_by(Calif_Final)

#se realiza grafica de barras de toda la poblacion de prepa y solo en mateáticas 1 por sexo



G1<-ggplot(calificaciones_prepa, aes(x=sexo))+geom_bar(fill="purple")+
ggtitle ("Todas las materias" )+ ylab("Alumnos")+ 
  theme(axis.title.x = element_text(size = 15), axis.text.x = element_text(size = 15))

G2 <-ggplot(Mate_1, aes(x=sexo))+geom_bar(fill="purple")+labs(title = "Matemáticas 1")+
  ylab("alumnos")+
  theme(axis.title.x = element_text(size = 15), axis.text.x = element_text(size = 15))


G3 <-ggplot(Lec, aes(x=sexo))+geom_bar(fill="purple")+labs(title = "Lectura y redacción")+
  ylab("alumnos")+
  theme(axis.title.x = element_text(size = 15), axis.text.x = element_text(size = 15))

wrap_plots(G1, G2,G3, ncol = 1, nrow = 3 )

#se obtiene media, desviacion estandar y mediana de matemáticas 1
Media_M <-mean(Mate_1$Calif_Final)
Desviacion_M <-sd(Mate_1$Calif_Final)
Mediana_M <-median(Mate_1$Calif_Final) 
print(paste("Promedio Calificacion Matemáticas I",Media_M))
print(paste("Desviación estandar Matemáticas I",Desviacion_M))
print(paste("Mediana de Matemáticas I",Mediana_M))

#se obtiene media, desviacion estandar y mediana de Lectura y redacción
Media_L <-mean(Lec$Calif_Final)
Desviacion_L <-sd(Lec$Calif_Final)
Mediana_L <-median(Lec$Calif_Final) 
print(paste("Promedio Lectura y redacción",Media_L))
print(paste("Desviación estandar Lectura y redacción",Desviacion_L))
print(paste("Mediana de Lectura y redacción",Mediana_L))
#se obtiene media, desviacion estnadar y mediana en matematicas por sexo M
Media_MM <-mean(Mate_m$Calif_Final)
Desviacion_MM <-sd(Mate_m$Calif_Final)
Mediana_MM <-median(Mate_m$Calif_Final) 
print(paste("Promedio Calificacion Matemáticas I de M",Media_MM))
print(paste("Desviación estandar Matemáticas I de M",Desviacion_MM))
print(paste("Mediana de Matemáticas I de M",Mediana_MM))

#se obtiene media, desviacion estnadar y mediana en matematicas por sexo F
Media_MF <-mean(Mate_f$Calif_Final)
Desviacion_MF <-sd(Mate_f$Calif_Final)
Mediana_MF <-median(Mate_f$Calif_Final) 
print(paste("Promedio Calificacion Matemáticas I de F",Media_MF))
print(paste("Desviación estandar Matemáticas I de F",Desviacion_MF))
print(paste("Mediana de Matemáticas I de F",Mediana_MF))

#se obtiene media, desviacion estnadar y mediana en lectura por sexo M
Media_LM <-mean(Lec_m$Calif_Final)
Desviacion_LM <-sd(Lec_m$Calif_Final)
Mediana_LM <-median(Lec_m$Calif_Final) 
print(paste("Promedio Calificacion Lectura y redacción de M",Media_LM))
print(paste("Desviación estandar Lectura y redacción de M",Desviacion_LM))
print(paste("Mediana de Lectura y redacción de M",Mediana_LM))

#se obtiene media, desviacion estnadar y mediana en lectura por sexo F
Media_LF <-mean(Lec_f$Calif_Final)
Desviacion_LF <-sd(Lec_f$Calif_Final)
Mediana_LF <-median(Lec_f$Calif_Final) 
print(paste("Promedio Calificacion Lectura y redacción de F",Media_LF))
print(paste("Desviación estandar Lectura y redacción de F",Desviacion_LF))
print(paste("Mediana de Lectura y redacción de F",Mediana_LF))
#obtenemos cuartiles de Matemáticas 1
print("Cuartiles Matemáticas I")
quantile(x=Mate_1$Calif_Final,probs = c(0.25, 0.50, 0.75), type = 0)

#obtenemos cuartiles de Lectura y redacción
print("Cuartiles Lectura y redacción")
quantile(x=Lec$Calif_Final,probs = c(0.25, 0.50, 0.75), type = 0)

#boxplot de matemáticas 1 y Lectuara y redacción
G4<-ggplot(data = Mate_1, mapping = aes(y=Calif_Final))+ geom_boxplot(outlier.colour = "red") + labs(title="Calificaciones de Matemáticas 1")
G5<-ggplot(data = Lec, mapping = aes(y=Calif_Final))+ geom_boxplot(outlier.colour = "blue") + labs(title="Calificaciones de Lectura y Redacción")
wrap_plots(G4, G5, ncol = 2, nrow = 1 )

#Histograma de matemáticas 1
par(mfrow=c(1,2))
hist(Mate_1$Calif_Final, main = "Distribucion de calificaciones Matemáticas 1",
     xlab="Calificacion",ylab = "Frecuencia", col = "red", border = "white")

hist(Lec$Calif_Final, main = "Distribucion de calificaciones Lectura y redacción",
     xlab="Calificacion",ylab = "Frecuencia", col = "red", border = "white")

par(mfrow=c(2,2))
hist(Mate_m$Calif_Final, main = "Distribucion de calificaciones Matemáticas 1 con M",
     xlab="Calificacion",ylab = "Frecuencia", col = "red", border = "white")

hist(Mate_f$Calif_Final, main = "Distribucion de calificaciones Matemáticas 1 con F",
     xlab="Calificacion",ylab = "Frecuencia", col = "red", border = "white")

hist(Lec_m$Calif_Final, main = "Distribucion de calificaciones Lectura y redacción 1 con M",
     xlab="Calificacion",ylab = "Frecuencia", col = "red", border = "white")

hist(Lec_f$Calif_Final, main = "Distribucion de calificaciones Lectura y redacción 1 con F",
     xlab="Calificacion",ylab = "Frecuencia", col = "red", border = "white")

par(mfrow=c(1,1))



par(mfrow=c(1,2))

#densidad de probabilidad para Matemáticas 1
x_m <-seq(0,20,length.out=100)
y_m <-dnorm(x_m, mean = Media_M, sd=Desviacion_M)
plot(x_m,y_m,type="l",lwd=2, col="red", 
     main = "Distribucion normal de Matemáticas 1",
     xlab="Calificaciones", ylab = "Densidad de probabiliodad")

#densidad de probabilidad para Lectura y redacción
x_l <-seq(0,20,length.out=100)
y_l <-dnorm(x_l, mean = Media_L, sd=Desviacion_L)
plot(x_l,y_l,type="l",lwd=2, col="red", 
     main = "Distribucion normal de Lectura y redacción",
     xlab="Calificaciones", ylab = "Densidad de probabiliodad")

#densidad de probabilidad para Matemáticas 1 M
x_mm <-seq(0,20,length.out=100)
y_mm <-dnorm(x_mm, mean = Media_MM, sd=Desviacion_MM)
plot(x_mm,y_mm,type="l",lwd=2, col="red", 
     main = "Distribucion normal de Matemáticas 1 M",
     xlab="Calificaciones", ylab = "Densidad de probabiliodad")
#densidad de probabilidad para Matemáticas 1 F
x_mf <-seq(0,20,length.out=100)
y_mf <-dnorm(x_mf, mean = Media_MF, sd=Desviacion_MF)
plot(x_mf,y_mf,type="l",lwd=2, col="red", 
     main = "Distribucion normal de Matemáticas 1 F",
     xlab="Calificaciones", ylab = "Densidad de probabiliodad")

#densidad de probabilidad para Lectura y redacción M
x_lm <-seq(0,20,length.out=100)
y_lm <-dnorm(x_lm, mean = Media_LM, sd=Desviacion_LM)
plot(x_lm,y_lm,type="l",lwd=2, col="red", 
     main = "Distribucion normal de Lectura y redacción M",
     xlab="Calificaciones", ylab = "Densidad de probabiliodad")

#densidad de probabilidad para Lectura y redacción F
x_lf <-seq(0,20,length.out=100)
y_lf <-dnorm(x_lf, mean = Media_LF, sd=Desviacion_LF)
plot(x_lf,y_lf,type="l",lwd=2, col="red", 
     main = "Distribucion normal de Lectura y redacción M",
     xlab="Calificaciones", ylab = "Densidad de probabiliodad")

par(mfrow=c(1,1))





#prueba de normalidad para la distribucion de Matemáticas 1
## Prueba de Normalidad Calificaciones Matemáticas: Shapiro-Wilk

PNormalidad_m <- shapiro.test(Mate_1$Calif_Final)
print(PNormalidad_m) #Un p-value menor a 0.05 indica que el conjunto de datos tiende a una distribución normal.

PNormalidad_l <- shapiro.test(Lec$Calif_Final)
print(PNormalidad_l) #Un p-value menor a 0.05 indica que el conjunto de datos tiende a una distribución normal.


PNormalidad_mm <- shapiro.test(Mate_m$Calif_Final)
print(PNormalidad_mm) #Un p-value menor a 0.05 indica que el conjunto de datos tiende a una distribución normal.

PNormalidad_lm <- shapiro.test(Lec_m$Calif_Final)
print(PNormalidad_lm) #Un p-value menor a 0.05 indica que el conjunto de datos tiende a una distribución normal.

PNormalidad_mf <- shapiro.test(Mate_1$Calif_Final)
print(PNormalidad_mf) #Un p-value menor a 0.05 indica que el conjunto de datos tiende a una distribución normal.

PNormalidad_lf <- shapiro.test(Lec$Calif_Final)
print(PNormalidad_lf) #Un p-value menor a 0.05 indica que el conjunto de datos tiende a una distribución normal.

par(mfrow=c(1,2))
#Gráfico de la distribución Matematicas 1
plotn_m <- function(x,main="Histograma de frecuencias \ny distribución normal",
                  xlab="Calificaciones",ylab="Densidad") {
  min_m <- min(Mate_1$Calif_Final)
  max_m <- max(Mate_1$Calif_Final)
  media_m <- mean(Mate_1$Calif_Final)
  dt_m <- sd(Mate_1$Calif_Final)
  hist(Mate_1$Calif_Final,freq=F,main=main,xlab=xlab,ylab=ylab)
  curve(dnorm(x,media_m,dt_m), min_m, max_m,add = T,col="red")
}

plotn_m(x,main="Distribución de Calificaciones Matemáticas") #Grafico de valores
############################################
############################################
############################################


#Gráfico de la distribució Lectura y redacción
plotn_l <- function(x,main="Histograma de frecuencias \ny distribución normal",
                    xlab="Calificaciones",ylab="Densidad") {
  min_l <- min(Lec$Calif_Final)
  max_l <- max(Lec$Calif_Final)
  media_l <- mean(Lec$Calif_Final)
  dt_l <- sd(Lec$Calif_Final)
  hist(Lec$Calif_Final,freq=F,main=main,xlab=xlab,ylab=ylab)
  curve(dnorm(x,media_l,dt_l), min_l, max_l,add = T,col="red")
}

plotn_l(x,main="Distribución de Calificaciones Lectura y redacción") #Grafico de valores
############################################
############################################
############################################

#Gráfico de la distribución Matematicas 1 M
plotn_mm <- function(x,main="Histograma de frecuencias \ny distribución normal",
                    xlab="Calificaciones",ylab="Densidad") {
  min_mm <- min(Mate_m$Calif_Final)
  max_mm <- max(Mate_m$Calif_Final)
  media_mm <- mean(Mate_m$Calif_Final)
  dt_mm <- sd(Mate_m$Calif_Final)
  hist(Mate_m$Calif_Final,freq=F,main=main,xlab=xlab,ylab=ylab)
  curve(dnorm(x,media_mm,dt_mm), min_mm, max_mm,add = T,col="red")
}

plotn_mm(x,main="Distribución de Calificaciones Matemáticas M") #Grafico de valores
############################################
############################################
############################################

#Gráfico de la distribución Matematicas 1 F
plotn_mf <- function(x,main="Histograma de frecuencias \ny distribución normal",
                    xlab="Calificaciones",ylab="Densidad") {
  min_mf <- min(Mate_f$Calif_Final)
  max_mf <- max(Mate_f$Calif_Final)
  media_mf <- mean(Mate_f$Calif_Final)
  dt_mf <- sd(Mate_f$Calif_Final)
  hist(Mate_f$Calif_Final,freq=F,main=main,xlab=xlab,ylab=ylab)
  curve(dnorm(x,media_mf,dt_mf), min_mf, max_mf,add = T,col="red")
}

plotn_mf(x,main="Distribución de Calificaciones Matemáticas F") #Grafico de valores
############################################
############################################
############################################

#Gráfico de la distribució Lectura y redacción M
plotn_lm <- function(x,main="Histograma de frecuencias \ny distribución normal",
                    xlab="Calificaciones",ylab="Densidad") {
  min_lm <- min(Lec_m$Calif_Final)
  max_lm <- max(Lec_m$Calif_Final)
  media_lm <- mean(Lec_m$Calif_Final)
  dt_lm <- sd(Lec_m$Calif_Final)
  hist(Lec_m$Calif_Final,freq=F,main=main,xlab=xlab,ylab=ylab)
  curve(dnorm(x,media_lm,dt_lm), min_lm, max_lm,add = T,col="red")
}

plotn_lm(x,main="Distribución de Calificaciones Lectura y redacción M") #Grafico de valores
############################################
############################################
############################################

#Gráfico de la distribució Lectura y redacción F
plotn_lf <- function(x,main="Histograma de frecuencias \ny distribución normal",
                    xlab="Calificaciones",ylab="Densidad") {
  min_lf <- min(Lec_f$Calif_Final)
  max_lf <- max(Lec_f$Calif_Final)
  media_lf <- mean(Lec_f$Calif_Final)
  dt_lf <- sd(Lec_f$Calif_Final)
  hist(Lec_f$Calif_Final,freq=F,main=main,xlab=xlab,ylab=ylab)
  curve(dnorm(x,media_lf,dt_lf), min_lf, max_lf,add = T,col="red")
}

plotn_lf(x,main="Distribución de Calificaciones Lectura y redacción F") #Grafico de valores
############################################
############################################
############################################


#boxplot de matemáticas 1 por sexo
G12<-ggplot(Mate_1,aes(x=sexo, y=Calif_Final))+geom_boxplot()+labs(title = "Matemáticas 1")+
  ylab("calificación")+
  theme(axis.title.x = element_text(size = 15), axis.text.x = element_text(size = 15))

G13<-ggplot(Lec,aes(x=sexo, y=Calif_Final))+geom_boxplot()+labs(title = "Lectura y redaccion")+
  ylab("calificación")+
  theme(axis.title.x = element_text(size = 15), axis.text.x = element_text(size = 15))

wrap_plots(G12, G13, ncol = 2, nrow = 1 )
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

