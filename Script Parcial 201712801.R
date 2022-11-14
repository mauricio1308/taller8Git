library(readxl)
library(dplyr)
library(plotly)
library(lubridate)
library(rriskDistributions)
library(fitdistrplus)
library(DT)
datos <- read_excel("201712801.xlsx")
colnames(datos)[1]<-"Fecha"
sabado<-filter(datos,day(Fecha)==27)
#Borrar el dato de las 8 pm
sabado<-sabado[1:263,]
domingo<-filter(datos,day(Fecha)==28)
#Borrar el dato de las 8 pm
domingo<-domingo[1:210,]
hist(sabado$Fecha,"hours",freq=TRUE,col="lightblue",main="Histograma llegadas día Sábado", ylab="Frecuencia",xlab="Hora")
hist(domingo$Fecha,"hours",freq=TRUE,col="lightblue",main="Histograma llegadas día Domingo",ylab="Frecuencia",xlab="Hora")
boxplot(difMinSab,difMinDom,col="lightblue",xlab="Día",ylab="Diferencia entre arribos (segundos)",main="Boxplot diferencia entre arribos día 1 (Sábado) día 2 (Domingo)")
#Crear franjas
f1sab<-filter(sabado,hour(Fecha)<=11)
f2sab<-filter(sabado,hour(Fecha)>11 & hour(Fecha)<=15)
f3sab<-filter(sabado,hour(Fecha)>15 & hour(Fecha)<=20)
#Eliminar datos atipicos y ajustar
fr1sab<-as.double(diff(f1sab$Fecha))
fr1sab<-fr1sab/60
hist(fr1sab,main="Diferencia entre llegadas Sábado Franja 1", xlab="Minutos",ylab="Frecuencia",freq=TRUE,col="lightblue")
fr1sab<-fr1sab[fr1sab>0 & fr1sab<5]
hist(fr1sab)
fit.cont(fr1sab) #Uniforme

fr2sab<-as.double((diff(f2sab$Fecha)))
fr2sab<-fr2sab/60
hist(fr2sab,main="Diferencia entre llegadas Sábado Franja 2", xlab="Minutos",ylab="Frecuencia",freq=TRUE,col="lightblue")
fr2sab<-fr2sab[fr2sab>0.5 & fr2sab<3]
hist(fr2sab)
fit.cont(fr2sab) #Uniforme

fr3sab<-as.double((diff(f3sab$Fecha)))
fr3sab<-fr3sab/60
fr3sab<-fr3sab[fr3sab>1 & fr3sab<3]
hist(fr3sab,main="Diferencia entre llegadas Sábado Franja 3", xlab="Minutos",ylab="Frecuencia",freq=TRUE,col="lightblue")
fit.cont(fr3sab) #Uniforme
acf(fr1sab)
acf(fr2sab)
acf(fr2sab)
#UNIR franja 1 y 2
f1dom<-filter(domingo,hour(Fecha)<=11)
f2dom<-filter(domingo,hour(Fecha)>11 & hour(Fecha)<=15)
f3dom<-filter(domingo,hour(Fecha)>15 & hour(Fecha)<=20)

acf(f1dom)
acf(f2dom)
acf(f3dom)

fr1dom<-as.double(diff(f1dom$Fecha))
fr1dom<-fr1dom[fr1dom>3 & fr1dom<5]
hist(fr1dom,main="Diferencia entre llegadas Domingo Franja 1", xlab="Minutos",ylab="Frecuencia",freq=TRUE,col="lightblue")
fit.cont(fr1dom) #Lognormal


fr2dom<-as.double((diff(f2dom$Fecha)))
fr2dom<-fr2dom/60
fr2dom<-fr2dom[fr2dom>3 & fr2dom<6]
fit.cont(fr2dom) #Lognormal
hist(fr2dom,main="Diferencia entre llegadas Domingo Franja 2", xlab="Minutos",ylab="Frecuencia",freq=TRUE,col="lightblue")
acf(fr2dom)

fr3dom<-as.double(diff(f3dom$Fecha))
fr3dom<-fr3dom/60
fr3dom<-fr3dom[fr3dom<8]
hist(fr3dom,main="Diferencia entre llegadas Domingo Franja 3", xlab="Minutos",ylab="Frecuencia",freq=TRUE,col="lightblue")
fit.cont(fr3dom) #Exponencial
acf(fr3dom)

boxplot(fr1dom,fr2dom,fr3dom)

hist(sabado$Fecha,"hours",freq=TRUE,main="Número de llegadas en cada hora día Sábado",xlab="Hora", col="lightblue",ylab="Frecuencia")
hist(domingo$Fecha,"hours",freq=TRUE,main="Número de llegadas en cada hora día Domingo", xlab="Hora",col="lightblue",ylab="Frecuencia")
plot_ly(x=~as.double(diff(sabado$Fecha)),type="histogram") %>% layout(title="Histograma tiempo entre arribos día sábado", xaxis=list(title="Diferencia entre arribos (segundos)"), yaxis=list(title="Frecuencia"))
plot_ly(x=~as.double(diff(domingo$Fecha)),type="histogram") %>% layout(title="Histograma tiempo entre arribos día domingo", xaxis=list(title="Diferencia entre arribos (segundos)"), yaxis=list(title="Frecuencia"))
#Leer los datos de tiempos de preparación  de combos
prepSandwich<-read_excel("201712801.xlsx",sheet="Sheet 2")
prepEnsaladaFrutas<-read_excel("201712801.xlsx",sheet="Sheet 3")
prepPanConHuevo<-read_excel("201712801.xlsx",sheet="Sheet 4")
prepCroissant<-read_excel("201712801.xlsx",sheet="Sheet 5")
prepEnsaladaVerduras<-read_excel("201712801.xlsx",sheet="Sheet 6")
prepSandwich<-prepSandwich[-10,]
prepSandwich<-prepSandwich[-122,]
#Leer los datos de  tiempos de consumo de combos
consSandwich<-read_excel("201712801.xlsx",sheet="Sheet 7")
consEnsaladaFrutas<-read_excel("201712801.xlsx",sheet="Sheet 8")
consPanConHuevo<-read_excel("201712801.xlsx",sheet="Sheet 9")
consCroissant<-read_excel("201712801.xlsx",sheet="Sheet 10")
consEnsaladaVerduras<-read_excel("201712801.xlsx",sheet="Sheet 11")
#Boxplot para mirar algunas estadisticas descriptivas de interés
plot_ly(y=prepSandwich$`Tiempo de Preparación (minutos)`,type="box",name="Sandwich") %>% add_trace(y=prepEnsaladaFrutas$`Tiempo de Preparación (minutos)`,type="box",name="Ensalada de frutas") %>% add_trace(y=prepPanConHuevo$`Tiempo de Preparación (minutos)`,type="box",name="Pan con huevo") %>% add_trace(y=prepCroissant$`Tiempo de Preparación (minutos)`,type="box",name="Croissant") %>% add_trace(y=prepEnsaladaVerduras$`Tiempo de Preparación (minutos)`,type="box",name="Ensalada de verduras") %>% layout(title="Box plot tiempo preparación combos (minutos)")
#Boxplot tiempo consumo combos
plot_ly(y=consSandwich$`Tiempo de Consumo (minutos)`,type="box",name="Sandwich") %>% add_trace(y=consEnsaladaFrutas$`Tiempo de Consumo (minutos)`,type="box",name="Ensalada de frutas") %>% add_trace(y=consPanConHuevo$`Tiempo de Consumo (minutos)`,type="box",name="Pan con huevo") %>% add_trace(y=consCroissant$`Tiempo de Consumo (minutos)`,type="box",name="Croissant") %>% add_trace(y=consEnsaladaVerduras$`Tiempo de Consumo (minutos)`,type="box",name="Ensalada verduras") %>% layout(title="Box-plot tiempo consumo combos (minutos)")
#Histogramas tiempos de preparación
plot_ly(x=prepSandwich$`Tiempo de Preparación (minutos)`,type="histogram") %>% layout(title="Histograma tiempo de preparación sandwich", xaxis=list(title="Tiempo preparación (minutos)"), yaxis=list(title="Frecuencia"))
fit.cont(prepSandwich$`Tiempo de Preparación (minutos)`)
plot_ly(x=prepEnsaladaFrutas$`Tiempo de Preparación (minutos)`,type="histogram") %>% layout(title="Histograma tiempo de preparación ensalada de frutas", xaxis=list(title="Tiempo preparación (minutos)"), yaxis=list(title="Frecuencia"))
fit.cont(prepEnsaladaFrutas$`Tiempo de Preparación (minutos)`)
ajusteprepEnsaladaF<-fitdist(prepEnsaladaFrutas$`Tiempo de Preparación (minutos)`,"gamma")
ajusteprepEnsaladaF$estimate
resultadosEnsaladaF <- gofstat(ajusteprepEnsaladaF)
resultadosEnsaladaF$chisqpvalue
plot(ajusteprepEnsaladaF)
plot_ly(x=prepPanConHuevo$`Tiempo de Preparación (minutos)`,type="histogram") %>% layout(title="Histograma tiempo de preparación pan con huevo", xaxis=list(title="Tiempo preparación (minutos)"), yaxis=list(title="Frecuencia"))
prepPanConHuevo<-filter(prepPanConHuevo,`Tiempo de Preparación (minutos)`>1.2 & `Tiempo de Preparación (minutos)`<10.13)
fit.cont(prepPanConHuevo$`Tiempo de Preparación (minutos)`) #exponencial
ajusteprepPanConHuevo<-fitdist(prepPanConHuevo$`Tiempo de Preparación (minutos)`,"lnorm")
ajusteprepPanConHuevo$estimate
plot(ajusteprepPanConHuevo)
resultadosPanConHuevo<-gofstat(ajusteprepPanConHuevo)
resultadosPanConHuevo$chisqpvalue
plot_ly(x=prepCroissant$`Tiempo de Preparación (minutos)`,type="histogram") %>% layout(title="Histograma tiempo de preparación croissant", xaxis=list(title="Tiempo preparación (minutos)"), yaxis=list(title="Frecuencia"))
fit.cont(prepCroissant$`Tiempo de Preparación (minutos)`)
ajusteprepCroissant<-fitdist(prepCroissant$`Tiempo de Preparación (minutos)`,"lnorm")
ajusteprepCroissant$estimate
plot(ajusteprepCroissant)
resultadosprepCroissant<-gofstat(ajusteprepCroissant)
resultadosprepCroissant$chisqpvalue
plot_ly(x=prepEnsaladaVerduras$`Tiempo de Preparación (minutos)`,type="histogram") %>% layout(title="Histograma tiempo de preparación ensalada de verduras", xaxis=list(title="Tiempo preparación (minutos)"), yaxis=list(title="Frecuencia"))
prepEnsaladaVerduras<-filter(prepEnsaladaVerduras,`Tiempo de Preparación (minutos)`>1.2 & `Tiempo de Preparación (minutos)`<12.42)
fit.cont(prepEnsaladaVerduras$`Tiempo de Preparación (minutos)`)
ajusteprepEnsaladaV<-fitdist(prepEnsaladaVerduras$`Tiempo de Preparación (minutos)`,"lnorm")
ajusteprepEnsaladaV$estimate
plot(ajusteprepEnsaladaV)
resultadosprepEnsaladaV<-gofstat(ajusteprepEnsaladaV)
resultadosprepEnsaladaV$chisqpvalue


#Histogramas tiempos de consumo
plot_ly(x=consSandwich$`Tiempo de Consumo (minutos)`,type="histogram") %>% layout(title="Histograma tiempo de consumo sandwich",xaxis=list(title="Tiempo consumo (minutos)"), yaxis=list(title="Frecuencia"))
consSandwich<-filter(consSandwich,`Tiempo de Consumo (minutos)`>2.21 &`Tiempo de Consumo (minutos)`<53.43)
ajusteConsSandwich<-fitdist(consSandwich$`Tiempo de Consumo (minutos)`,"lnorm")
ajusteConsSandwich$estimate
plot(ajusteConsSandwich)
resultadosConsSandwich<-gofstat(ajusteConsSandwich)
resultadosConsSandwich$chisqpvalue
plot_ly(x=consEnsaladaFrutas$`Tiempo de Consumo (minutos)`,type="histogram") %>% layout(title="Histograma tiempo de consumo ensalada de frutas",xaxis=list(title="Tiempo consumo (minutos)"), yaxis=list(title="Frecuencia"))
ajusteconsEnsaladaFrutas<-fitdist(consEnsaladaFrutas$`Tiempo de Consumo (minutos)`,"unif")
ajusteconsEnsaladaFrutas$estimate
plot(ajusteconsEnsaladaFrutas)
resultadosconsEnsaladaF<-gofstat(ajusteconsEnsaladaFrutas)
resultadosconsEnsaladaF$chisqpvalue
plot_ly(x=consPanConHuevo$`Tiempo de Consumo (minutos)`,type="histogram") %>% layout(title="Histograma tiempo de consumo pan con huevo",xaxis=list(title="Tiempo consumo (minutos)"), yaxis=list(title="Frecuencia"))
ajusteconsPan<-fitdist(consPanConHuevo$`Tiempo de Consumo (minutos)`,"lnorm")
ajusteconsPan$estimate
plot(ajusteconsPan)
resultadosConsPan<-gofstat(ajusteconsPan)
resultadosConsPan$chisqpvalue
plot_ly(x=consCroissant$`Tiempo de Consumo (minutos)`,type="histogram") %>% layout(title="Histograma tiempo de consumo croissant",xaxis=list(title="Tiempo consumo (minutos)"), yaxis=list(title="Frecuencia"))
fit.cont(consCroissant$`Tiempo de Consumo (minutos)`)
ajusteconsCros<-fitdist(consCroissant$`Tiempo de Consumo (minutos)`,"weibull")
ajusteconsCros$estimate
plot(ajusteconsCros)
resultadosconsCros<-gofstat(ajusteconsCros)
resultadosconsCros$chisqpvalue
plot_ly(x=consEnsaladaVerduras$`Tiempo de Consumo (minutos)`,type="histogram") %>% layout(title="Histograma tiempo de consumo ensalada verduras",xaxis=list(title="Tiempo consumo (minutos)"), yaxis=list(title="Frecuencia"))
ajusteconsEnsaladaVerd<-fitdist(consEnsaladaVerduras$`Tiempo de Consumo (minutos)`,"weibull")
ajusteconsEnsaladaVerd$estimate
plot(ajusteconsEnsaladaVerd)#Indicios
var.test(prepSandwich$`Tiempo de Preparación (minutos)`,prepEnsaladaFrutas$`Tiempo de Preparación (minutos)`,alternative="two.sided")
var.test(prepPanConHuevo$`Tiempo de Preparación (minutos)`,prepEnsaladaVerduras$`Tiempo de Preparación (minutos)`,alternative="two.sided")
#Consumo pan con huevo y ensalada verduras HOMOGENEOS
var.test(consPanConHuevo$`Tiempo de Consumo (minutos)`,consEnsaladaVerduras$`Tiempo de Consumo (minutos)`,alternative="two.sided")
t.test(consPanConHuevo$`Tiempo de Consumo (minutos)`,consEnsaladaVerduras$`Tiempo de Consumo (minutos)`,alternative = "two.sided",mu=0,var.equal=TRUE,conf.level = 0.95)
#Consumo ensalada frutas y croissant HOMOGENEOS
var.test(consEnsaladaFrutas$`Tiempo de Consumo (minutos)`,consCroissant$`Tiempo de Consumo (minutos)`,alternative = "two.sided")
t.test(consEnsaladaFrutas$`Tiempo de Consumo (minutos)`,consCroissant$`Tiempo de Consumo (minutos)`,alternative="two.sided",mu=0,var.equal = TRUE,conf.level=0.95)

datosLocal<-filter(datos,`Tipo de Pedido`=="En local")
datosRappi<-filter(datos,`Tipo de Pedido`=="Rappi")
round(table(datosLocal$`Número de Combos`)/nrow(datosLocal),3)
round(table(datosRappi$`Número de Combos`)/nrow(datosRappi),3)
table(datosLocal$`Número de Combos`)
table(datosRappi$`Número de Combos`)
