


arsenicoOrina=c(41.75,114.84,107.58,29.93,73.46,101.6,49.47,40.78,57.7,53.24,57.43,58.43,50.56,47.91,24.79,93.85,54.75,43.81,54.17)

summary((arsenicoOrina))
boxplot(arsenicoOrina)


##############
# 
#An�lisis de Dispersi�n
#
##############

hist(arsenicoOrina,probability = TRUE) #Este es para mostrarlo en funci�n de la probabilidad.
lines(density(arsenicoOrina))

#Se puede observar 2 puntos de acumulaci�n, se sale de los l�mites superiores de ars�nico
#en los pacientes, se puede pensar que hay un valor relativamente normal
#o de acumulaci�n despues de los 100 puntos de ars�nico en la orina
#
stand=sd(arsenicoOrina)
#El promedio debe estar tentativamente en el centro, pero depende de la distribuci�n
#de los datos. El primer momento es el promedio, la desviaci�n est�ndard =25 es el segundo momento en la generadora
#de momentos, lo que nos dice el segundo momento es que el 66% de la 
#poblaci�n est� entre 25 y 79 unidades. Para sacar el 95% de la poblaci�n se le debe de sumar y restar
#2 desviaciones est�ndard a la media, en este ejemplo como la media est� en 60.04, el 95%
#Est� entre 10 y 100. se calcula tmbn con P value
#El segundo momento nos dice qu� dispersos est�n.

library(e1071)
skew=skewness(arsenicoOrina)#Se utiliza la librer�a e1071

#El tercer momento (sesgo) nos da positivo, esto quiere decir que
#esta sesgado a la derecha, eso quiere decir que hay pocos datos en la derecha
#En una distribucion normal el sesgo equivale a 0 en un caso ideal

kurt=kurtosis(arsenicoOrina)

#Cuarto Momento, la curtosis puede est�r en funci�n de 3: 3>x<3 o de 0.


##

#Correlaci�n


#####
#Tener correlaci�n no implica causalidad, pero si indica una relaci�n entre ellas
#r de -1 hasta 1

arsenicoAgua=c(36.78,48.5,39.42,24.45,32.15,40.46,43.68,43.01,39.2,39.79, 
               42.3,46.63,44.39,35.58,39.2,38.28,43.34,36.69,41.78)

summary(arsenicoAgua)

hist(arsenicoAgua)
sd(arsenicoAgua)

#Graficar 2 variables juntas
par(mfrow=c(1,2))
hist(arsenicoOrina,probability=TRUE)
lines(density(arsenicoOrina))
hist(arsenicoAgua,probability=TRUE)
lines(density(arsenicoAgua))

#Correlaci�n asumiendo normalidad
cor.test(arsenicoOrina,arsenicoAgua)
cor.test(arsenicoAgua,arsenicoOrina,method="spearman")



#Correlaci�n de kendal, variables clasificatorias
#correlaci�n de spearman, extremadamente sesgados a la derecha o a la izquierda, 
#evaluamos los cambios de la evoluci�n
#

plot(arsenicoAgua,type="o",col="blue",pch="o",lty=1,ylim=c(0,110))
lines(arsenicoOrina,type="o",col="green",pch="o",lty=1,ylim=c(0,110))
#t es un factor de ajuste
#df 
#el evento p-value nos indica el azar, si tu repites el evento 100 veces, a lo
#mejor 19 te van a dar una correlaci�n relativa
#Teorema de l�mite central
#df

#spearman est� asumiendo q no es normal el comportamiento, es por eso que
#no te da el p-value

shapiro.test(arsenicoAgua)
#w=
#p-value= es la probabilidad de que el conjunto de datos sea normal
#en este caso es al rev�s, es decir, con .1626 es un valor bajo pero normal si es 
#<.05, 
shapiro.test(arsenicoOrina)
#p-value=0.019, Ni si quiera el 2% de las veces tiene un valor normal


#Primero se hacen estas 2 pruebas, se puede ver q en el primero si es un valor
#normal (arsenicoAgua) pero en el otro no, entonces hay que justificar
#la relaci�n positiva.

boxplot(arsenicoAgua)

tsh=c(7.47,2.64,-2.49,0.31,7.47,6.64,13.49,2.64,14.45,7.27,17.31,3.45,0.57,
      7.84,7.08,1.25,6.69,0.64,1.06)
shapiro.test((tsh)) 
#Significa que el 12.81% de los casos se tiene un valor normal
#si tiene m�s de 0.5 es porque es un evento normal.
#es demostrable que este fen�meno es normal.

boxplot(tsh)
summary(tsh)
hist(tsh,probability=TRUE)
lines(density(tsh))

kurtosis(tsh)
sd(tsh)
skewness(tsh)
#los datos se encuentran m�s hacia la izquierda de la media

cor.test(tsh,arsenicoAgua)
#El valor de p es extremadamente alto, por lo que no
#hay relaci�n entre ellos
#
cor.test(tsh,arsenicoOrina)
cor.test(tsh,arsenicoOrina,method="spearman")
# el valor de P es muy alto, que pacho con p? el intervalo de confianza
# es muy alto, el p value est� diciendo la probabilidad
# de que lo que est�s viendo es al azar, es decir, tus muestras pueden ser
# una coinidencia.


#media=5.567+5.28
#T 0-4.5 Saludable
# 4.5-10 alteraci�n
# 10-15 grave
#el 95% de las observaciones est�n entre 16.127

el95=5.567+2*5.28
el5=5.567-2*5.28


###### Correlaci�n m�ltiple

#######crear data set
datosArsenico=cbind.data.frame(arsenicoOrina,arsenicoAgua,tsh)
View(datosArsenico)
matrizDeCorPearson=cor(datosArsenico)
matrizDeCorSpearman=cor(datosArsenico,method='spearman')
heatmap(matrizDeCorPearson)
heatmap(matrizDeCorSpearman)
library(corrplot)

corrplot(matrizDeCorPearson,method = 'number')

corrplot(matrizDeCorPearson,method = 'pie')
corrplot(matrizDeCorPearson,method = 'number',type = 'upper',col = FALSE,bg='red')
#col= colores, si es true, el color 
corrplot(matrizDeCorPearson,method = 'number',diag = FALSE)

corrplot(matrizDeCorPearson,method = 'pie',outline = T)
#                                                                                     Modo porcentaje           #no entiendo pa q es xd
corrplot(matrizDeCorPearson,method = 'pie',addgrid.col = 'green',addCoef.col = 'blue',addCoefasPercent = T, hclust.method = 'average')

#add: si es igual a TRUE, se a�ade a un plot existente.
#mar
#addgrid.col color de las lineas.

#en el modelo de regresi�n lineal hay que evitar los outliers
#ya qu la mayor�a de las veces se forma un desbalance


######Regresi�n lineal

modeloAAO=lm(arsenicoOrina ~ arsenicoAgua)
lm(formula=arsenicoOrina~arsenicoAgua)
##Una persona que est� en el pozo tiene 1.847 en la orina cuando las
#unidades en el agua est� en 0.
#Intercept x=0
#El siguiente dato nos dice que que hay un factor de cambio de 1.48 unidades por cada unidad 
#que avance X

#(1.8+14.84(10 instancias))/8
#

summary(modeloAAO)
#El promedio es q se equivoca con 8 unidades
#Intercept, nivel base de la poblaci�n 1.82, el error est�ndar
#el valor d p, es una constante, no interesan estos valores
#Estima un valor de incremento con 1.48 unidades, el valor de t
#depende completamente del numero de muestras

##p debe de ser menor a 0.05, si es mayor a �ste, quiere decir
 # que probablemente sea el valor negativo aunque est� en positivo
##

#Residuo standard,
#Valor de R cuadrada: capacidad de modelar la varianza
#te dice qu� tan bueno es el modelo, lo ideal es q llegara a 1
#, la R ajustada 
par(mfrow=c(1,1))
plot(arsenicoAgua,arsenicoOrina,pch=16,cex=1.3,col='green')
abline(modeloAAO)

prediccion=modeloAAO$coefficients[1] + modeloAAO$coefficients[2]*15#Estimaci�n de una persona si tiene 15
#unidades
prediccion

#Modelo para estimar la LTSH
modelAOTSH=lm(tsh ~ arsenicoOrina)
lm(formula=tsh~arsenicoOrina)

summary(modelAOTSH)
plot(arsenicoOrina,tsh,pch=16,cex=1.3,col='green')
abline(modelAOTSH)


######Multiple
modeloTSHM=lm(tsh~arsenicoOrina+arsenicoAgua)
modeloTSHM
lm(formula=tsh~arsenicoOrina+arsenicoAgua)

summary(modeloTSHM)
plot(modeloTSHM)
###
###
termplot(modeloTSHM)
###Para cada una de las variables, representa la correlaci�n entre una parte de la misma
#variable
fluorOrina=c(2.72,2.54,2.6,2.57,1.24,1.23,2.48,2.63,2.69,2.54,
             2.6, 1.2, 2.66, 2.69, 2.69, 2, 1.9, 1.1, 1.25
             )

datosArsenico=cbind.data.frame(arsenicoOrina,arsenicoAgua,tsh,fluorOrina)
View(datosArsenico)
matrizDeCorPearson=cor(datosArsenico)
matrizDeCorSpearman=cor(datosArsenico,method='spearman')
corrplot(matrizDeCorPearson,method='number',col='yellow',bg='red')
heatmap(matrizDeCorSpearman)

model4=lm(tsh~arsenicoOrina+arsenicoAgua+fluorOrina)
model4
summary(model4)

####Modelo de varianza
aovTSHM=aov(tsh~arsenicoOrina+arsenicoAgua+fluorOrina)
aovTSHM
summary(aovTSHM)
#cu�nto est� modelando F value es la parte del modelo q no se ha modelado
#No toma en cuenta las partes que ya han tomado en cuenta
dental_0_imputed <- read.csv("~/programas R/mineria de datos/dental_0_imputed.csv")


##Otros modelos de regresi�n
#######
########
#######

library(rminer)
#fat
#fat
#fat
#fat


dat=datos19corregida
set.seed(111)
modeloTSHRF=fit(dat$Tsh~.,data=dat,model='randomForest',nTree='500')
modeloTSHRF
plot(modeloTSHRF@object)

P=predict(modeloTSHRF,dat)
pe=P-dat$Tsh

mean(abs(pe))

dental_0_imputed$caries[dental_0_imputed$caries==2]<-1

modeloDental=fit(dental_0_imputed[1:1000,1:100]$caries~.,
                 data=as.data.frame(dental_0_imputed[1:1000,]),model='randomForest',nTree='800')
modeloDental
plot(modeloDental@object)
p=predict(modeloDental,dental_0_imputed[1001:9000,])
sum(abs(round(p)-dental_0_imputed[1001:9000,]$caries))
table(dental_0_imputed$caries)

H=holdout(dental_0_imputed$caries,ratio=2/3,mode = 'stratified')
H
modeloDental=fit(dental_0_imputed[H$tr,1:100]$caries~.,
                 data=as.data.frame(dental_0_imputed[H$tr,1:100]),model='randomForest',nTree='100')

plot(modeloDental@object)
p=predict(modeloDental,dental_0_imputed[1001:9000,])
sum(abs(round(p)-dental_0_imputed[1001:9000,]$caries))
table(dental_0_imputed$caries)

##################
#
# Un enfoque no supervisado
#
##################

##################
#
# Hierarchical clustering
#
##################

modeloHDental=dist(dental_0_imputed[1:100,])
hcDental=hclust(modeloHDental)
hcDental
plot(hcDental)
rect.hclust(hcDental,k=5,border='red')

library(ape)
plot(as.phylo(hcDental),type = 'cladogram')

plot(as.phylo(hcDental),type = 'fan')
colors=c('green','blue','red','orange','purple')
hc3=cutree(hcDental,5)

plot(as.phylo(hcDental),type = 'fan',tip.color=colors[hc3])



