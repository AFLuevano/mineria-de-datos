


# model=lm(Tsh~.,data = datos19corregida)
trainobs=as.integer(0.7*length(datos19corregida$Tsh))
# th=model$coefficients

for (i in th){
  for (j in datos19corregida$Tsh){
  Jtrain<-1/(2*length(datos19corregida$Arsenico_en_orina))*(sum(th-j))^2
  }
}

# Jtrain=c(0)
# for (i in 2:length(datos19corregida))
# {
#   baseparcial<-datos19corregida[1:trainobs,1:i]
#   m<-lm(Tsh~.,data=baseparcial)
#   th=m$coefficients
#   for (j in baseparcial$Tsh){
#     Jtrain[i]<-1/(2*length(trainobs))*(sum(th-j))^2
#   }
# }
# Jtrain
# plot(Jtrain)


#Cuando hay un incremento o un "escalon" quiere decir q esas características entorpecen nuestro modelo
#es por ello que se deben de eliminar, además de que después de ciertas características existe un
#sobreajuste, lo que llega a incluso incrementar después de cierto punto.


