dental_0_imputed$caries[dental_0_imputed$caries==2]<-1
datos=dental_0_imputed
library(pROC)
colnames(datos)
for(i in c(3:ncol(datos))){
  name=colnames(datos[i])
  print(name)
}
resultados=data.frame(Feature='x',AUC=0)
for(i in c(3:ncol(datos))){
  # name=colnames(datos[i])
  datos2=datos[c(2,i)]
  modelo=glm(caries~., data = datos2,family = 'binomial')
  
  predicciones_m<-predict(modelo,newdata=datos,
                                     type="response")
  
  
  
  tablita<-data.frame(Original=datos$caries,
                      Predicciones=predicciones_m)
  
  # tablita$Predicciones[tablita$Predicciones<0.5]<-0
  # tablita$Predicciones[tablita$Predicciones>=0.5]<-1
  
  curva<-roc(response=tablita$Original, predictor=tablita$Predicciones,
             levels=c(0,1),
             plot=T,
             ci=T, #confidence interval
             smooth=F,
             direction='auto')
  
  resultados=rbind(resultados,c(colnames(datos2)[2],curva$auc))
  
}
  
