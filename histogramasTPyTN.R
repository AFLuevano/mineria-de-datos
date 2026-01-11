#Los datos para que nos puedan servir debemos de analizar los datos 
#con una gráfica de errores entre los datos de entrenamiento y de prueba.


# for(i in 2:trainObs)
# {
#   baseParcial<-clases[1:i,]
#   m<-lm(caries~.,data=baseParcial)
#   residuos[i]<-mean(abs(m$residuals))
#   baseParcial<-clases[trainObs:nrow(clases),]
#   a=predict(m,baseParcial)
#   residuosTest[i]<-mean(abs(a-baseParcial$caries))
# }
# 
# plot(log(residuosTest),type='l',col='red')
# lines(log(residuos),col='green')

##histograma de verdaderos positivos
library(rminer)

dental_0_imputed$caries[dental_0_imputed$caries==2]<-1
modeloDental=fit(dental_0_imputed[1:1000,1:100]$caries~.,
                 data=as.data.frame(dental_0_imputed[1:1000,]),model='randomForest',nTree='800')


plot(modeloDental@object)#

p=predict(modeloDental,dental_0_imputed[1001:9000,])
sum(abs(round(p)-dental_0_imputed[1:1000,]$caries))

library(caret)

#Vamos a utilizar una funcion que tiene la libreria caret
tablita<-data.frame(Original=dental_0_imputed[1:1000,]$caries,
                     Predicciones=p)

TN<-tablita$Predicciones[tablita$Original==0]
TP<-tablita$Predicciones[tablita$Original==1]

hist(TN,probability=TRUE)
lines(density(TN))
hist(TP,probability=TRUE)
lines(density(TP))

library(e1071)

lines(seq(0,5,length=100),dnorm(seq(0,5,length=100),mean=mean(TP),sd=sd(TP)),lwd=3)

lines(seq(0,5,length=100),dnorm(seq(0,5,length=100),mean=mean(TN),sd=sd(TN)),lwd=3)
