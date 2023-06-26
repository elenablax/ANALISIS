#ANÁLISIS DE UNA BASE DE DATOS QUE CONTIENE INFORMACIÓN ACERCA DE 24 COMPONENTES NUTRICIONALES MEDIDAS SOBRE 31 VARIEDADES DE QUESO

#PREPARACIÓN DE LA BASE DE DATOS

#Modificación de la base de datos para que la primera columna contenga los nombres de las variedades de queso
install.packages("dplyr")
library(dplyr)

rownames(queso_final)<-queso_final$...1 
nombres<-(rownames(queso_final))
queso_sin<-select(queso_final,-1)
rownames(queso_sin)<-nombres


#ANÁLISIS DESCRIPTIVO 

install.packages("e1071")
library(e1071)

summary(queso_sin)#cálculo del mínimo, máximo, mediana y media de todas las componentes nutricionales
sapply(queso_sin,sd)#cálculo de la desviación típica de las componentes nutricionales
sapply(queso_sin,IQR)#cálculo del rango intercuartílico de las componentes nutricionales
sapply(queso_sin,skewness)#cálculo del coeficiente de asimetría de Pearson de las componentes nutricionales


#ANÁLISIS MULTIVARIANTE 

#Estandarización y centrado de los datos

queso<-as.data.frame(scale(queso_sin))


#Cálculo y representación de la matriz de correlaciones entre componentes nutricionales

install.packages("corrplot")
library(corrplot)

matrizcor<-cor(queso)#matriz de correlaciones
print(matrizcor)
corrplot(matrizcor,method="circle",tl.col = 'black',col = COL2('PRGn'),main="Correlaciones entre componentes nutricionales")#representación gráfica de la matriz de correlaciones


#PCA

install.packages("factoextra")
library(factoextra)

pca1<-prcomp(queso)#PCA de la base de datos
print(pca1)#matriz de cargas de las componentes principales construidas
summary(pca1)#desviación típica, varianza explicada y varianza explicada acumulada de cada una de las componentes principales
pca1$sdev^2#valores propios asociados a las componentes principales
fviz_pca_ind(pca1, geom.ind = c("text"), cex.lab=0.7, col.ind = "magenta",axes = c(1,2), xlim=c(-8,5),ylim=c(-2.5,7),labelsize=3)#proyección de las variedades de queso sobre las dos primeras componentes principales


#Sparse PCA

install.packages("elasticnet")
library(elasticnet)

spca1<-spca(queso,2,para=c(12,12),type="predictor",sparse="penalty")#Sparse PCA de la base de datos
print(spca1)#matriz de cargas de las componentes principales sparse 
spca1$pev# varianza explicada por las componentes principales sparse


#Métodos Biplot

install.packages("biplotbootGUI")
library(biplotbootGUI)

biplotboot(queso)#representación del HJ-Biplot de las variedades de queso a través de una ventana interactiva emergente





