library(vcd)
library(MASS)
library(ggplot2)

library(fitdistrplus)
library(logspline)

##
#Leo los datos
##
data2.0<-read.csv("DatosAjustados.csv")
matdata2.0=as.matrix(data2.0)


##
#Estacion carvajal
##
carvajal <- vector(mode="numeric")
cont=0
carvajal2<-vector(mode="numeric")
cont2=0

#Hallo el vector de tiempos de las llegadas
for(i in 1:length(matdata2.0[,"FECHA"])){
  datoActual=as.numeric(matdata2.0[i,2])
  if(datoActual==1){
    cont=cont+1
    fechaActual=matdata2.0[i,1]
    carvajal[cont]=fechaActual
  }
}
#Hallo el vector de tiempos entre llegadas
for(i in 2:length(carvajal)){
  cont2=cont2+1
  carvajal2[cont2]=carvajal[i]-carvajal[i-1]
}

carvajalSinUnos<-carvajal2>1
carvajal3<-carvajal2[carvajalSinUnos]

#

descdist(carvajal3,discrete = FALSE)

#Hago un fit para mi distribucion y hallo mi rate carvajal
fit.carvajal<-fitdist(carvajal3, "gamma")
fit.carvajal
shape.carvajal<-fit.carvajal$estimate[1]
rate.carvajal<-fit.carvajal$estimate[2]

##
##
##
#Estacion car
##
car <- vector(mode="numeric")
cont=0
car2<-vector(mode="numeric")
cont2=0

#Hallo el vector de tiempos de las llegadas
for(i in 1:length(matdata2.0[,"FECHA"])){
  datoActual=as.numeric(matdata2.0[i,3])
  if(datoActual==1){
    cont=cont+1
    fechaActual=matdata2.0[i,1]
    car[cont]=fechaActual
  }
}
#Hallo el vector de tiempos entre llegadas
for(i in 2:length(car)){
  cont2=cont2+1
  car2[cont2]=car[i]-car[i-1]
}

carSinUnos<-car2>1
car3<-car2[carSinUnos]

descdist(car3,discrete = FALSE)

#Hago un fit para mi distribucion y hallo mi rate car
fit.car<-fitdist(car3, "gamma")
shape.car<-fit.car$estimate[1]
rate.car<-fit.car$estimate[2]


##
#Estacion guaymaral
##
guaymaral <- vector(mode="numeric")
cont=0
guaymaral2<-vector(mode="numeric")
cont2=0

#Hallo el vector de tiempos de las llegadas
for(i in 1:length(matdata2.0[,"FECHA"])){
  datoActual=as.numeric(matdata2.0[i,4])
  if(datoActual==1){
    cont=cont+1
    fechaActual=matdata2.0[i,1]
    guaymaral[cont]=fechaActual
  }
}
#Hallo el vector de tiempos entre llegadas
for(i in 2:length(guaymaral)){
  cont2=cont2+1
  guaymaral2[cont2]=guaymaral[i]-guaymaral[i-1]
}

guaymaralSinUnos<-guaymaral2>1
guaymaral3<-guaymaral2[guaymaralSinUnos]

descdist(guaymaral3,discrete = FALSE)
#
#Hago un fit para mi distribucion y hallo mi rate gauymaral
fit.guaymaral<-fitdist(guaymaral3, "gamma")
shape.guaymaral<-fit.guaymaral$estimate[1]
rate.guaymaral<-fit.guaymaral$estimate[2]
plot(fit.guaymaral)

##
#Estacion Kennedy
##
Kennedy <- vector(mode="numeric")
cont=0
Kennedy2<-vector(mode="numeric")
cont2=0

#Hallo el vector de tiempos de las llegadas
for(i in 1:length(matdata2.0[,"FECHA"])){
  datoActual=as.numeric(matdata2.0[i,5])
  if(datoActual==1){
    cont=cont+1
    fechaActual=matdata2.0[i,1]
    Kennedy[cont]=fechaActual
  }
}
#Hallo el vector de tiempos entre llegadas
for(i in 2:length(Kennedy)){
  cont2=cont2+1
  Kennedy2[cont2]=Kennedy[i]-Kennedy[i-1]
}

KennedySinUnos<-Kennedy2>1
Kennedy3<-Kennedy2[KennedySinUnos]

descdist(Kennedy3,discrete = FALSE)
#Hago un fit para mi distribucion y hallo mi rate de kennedy
fit.kennedy<-fitdist(Kennedy3, "gamma")
shape.kennedy<-fit.kennedy$estimate[1]
rate.kennedy<-fit.kennedy$estimate[2]

#

##
#Estacion lasferias
##
lasferias <- vector(mode="numeric")
cont=0
lasferias2<-vector(mode="numeric")
cont2=0

#Hallo el vector de tiempos de las llegadas
for(i in 1:length(matdata2.0[,"FECHA"])){
  datoActual=as.numeric(matdata2.0[i,6])
  if(datoActual==1){
    cont=cont+1
    fechaActual=matdata2.0[i,1]
    lasferias[cont]=fechaActual
  }
}
#Hallo el vector de tiempos entre llegadas
for(i in 2:length(lasferias)){
  cont2=cont2+1
  lasferias2[cont2]=lasferias[i]-lasferias[i-1]
}

lasferiasSinUnos<-lasferias2>1
lasferias3<-lasferias2[lasferiasSinUnos]


descdist(lasferias3,discrete = FALSE)
#
#Hago un fit para mi distribucion y hallo mi rate lasferias
fit.lasferias<-fitdistr(lasferias3,"Exponential")
rate.lasferias<-fit.lasferias$estimate
#1.
#Realizo un histograma y una grafica exponencial con mi rate
hist(lasferias3, freq = FALSE)
curve(dexp(x, rate = rate.lasferias), col = "red", add = TRUE)
#2.
#Hago el test KS para saber si se distribuye exponencial
ks.test(lasferias3,"pexp",rate.lasferias)

##
#Estacion minambiente
##
minambiente <- vector(mode="numeric")
cont=0
minambiente2<-vector(mode="numeric")
cont2=0

#Hallo el vector de tiempos de las llegadas
for(i in 1:length(matdata2.0[,"FECHA"])){
  datoActual=as.numeric(matdata2.0[i,7])
  if(datoActual==1){
    cont=cont+1
    fechaActual=matdata2.0[i,1]
    minambiente[cont]=fechaActual
  }
}
#Hallo el vector de tiempos entre llegadas
for(i in 2:length(minambiente)){
  cont2=cont2+1
  minambiente2[cont2]=minambiente[i]-minambiente[i-1]
}

minambienteSinUnos<-minambiente2>1
minambiente3<-minambiente2[minambienteSinUnos]


descdist(minambiente3,discrete = FALSE)
#
#Hago un fit para mi distribucion y hallo mi rate minambiente
fit.minambiente<-fitdistr(minambiente3,"Exponential")
rate.minambiente<-fit.minambiente$estimate
#1.
#Realizo un histograma y una grafica exponencial con mi rate
hist(minambiente3, freq = FALSE)
curve(dexp(x, rate = rate.minambiente), col = "red", add = TRUE)
#2.
#Hago el test KS para saber si se distribuye exponencial
ks.test(minambiente3,"pexp",rate.minambiente)

##
#Estacion puentearanda
##
puentearanda <- vector(mode="numeric")
cont=0
puentearanda2<-vector(mode="numeric")
cont2=0

#Hallo el vector de tiempos de las llegadas
for(i in 1:length(matdata2.0[,"FECHA"])){
  datoActual=as.numeric(matdata2.0[i,8])
  if(datoActual==1){
    cont=cont+1
    fechaActual=matdata2.0[i,1]
    puentearanda[cont]=fechaActual
  }
}
#Hallo el vector de tiempos entre llegadas
for(i in 2:length(puentearanda)){
  cont2=cont2+1
  puentearanda2[cont2]=puentearanda[i]-puentearanda[i-1]
}

puentearandaSinUnos<-puentearanda2>1
puentearanda3<-puentearanda2[puentearandaSinUnos]

descdist(puentearanda3,discrete = FALSE)
#
#Hago un fit para mi distribucion y hallo mi rate puentearanda
fit.puentearanda<-fitdistr(puentearanda3,"Exponential")
rate.puentearanda<-fit.puentearanda$estimate
#1.
#Realizo un histograma y una grafica exponencial con mi rate
hist(puentearanda3, freq = FALSE)
curve(dexp(x, rate = rate.puentearanda), col = "red", add = TRUE)
#2.
#Hago el test KS para saber si se distribuye exponencial
ks.test(puentearanda3,"pexp",rate.puentearanda)

##
#Estacion sancristobal
##
sancristobal <- vector(mode="numeric")
cont=0
sancristobal2<-vector(mode="numeric")
cont2=0

#Hallo el vector de tiempos de las llegadas
for(i in 1:length(matdata2.0[,"FECHA"])){
  datoActual=as.numeric(matdata2.0[i,9])
  if(datoActual==1){
    cont=cont+1
    fechaActual=matdata2.0[i,1]
    sancristobal[cont]=fechaActual
  }
}
#Hallo el vector de tiempos entre llegadas
for(i in 2:length(sancristobal)){
  cont2=cont2+1
  sancristobal2[cont2]=sancristobal[i]-sancristobal[i-1]
}

sancristobalSinUnos<-sancristobal2>1
sancristobal3<-sancristobal2[sancristobalSinUnos]

#
#Hago un fit para mi distribucion y hallo mi rate sancristobal
fit.sancristobal<-fitdistr(sancristobal3,"Exponential")
rate.sancristobal<-fit.sancristobal$estimate
#1.
#Realizo un histograma y una grafica exponencial con mi rate
hist(sancristobal3, freq = FALSE)
curve(dexp(x, rate = rate.sancristobal), col = "red", add = TRUE)
#2.
#Hago el test KS para saber si se distribuye exponencial
ks.test(sancristobal3,"pexp",rate.sancristobal)


##
#Estacion suba
##
suba <- vector(mode="numeric")
cont=0
suba2<-vector(mode="numeric")
cont2=0

#Hallo el vector de tiempos de las llegadas
for(i in 1:length(matdata2.0[,"FECHA"])){
  datoActual=as.numeric(matdata2.0[i,10])
  if(datoActual==1){
    cont=cont+1
    fechaActual=matdata2.0[i,1]
    suba[cont]=fechaActual
  }
}
#Hallo el vector de tiempos entre llegadas
for(i in 2:length(suba)){
  cont2=cont2+1
  suba2[cont2]=suba[i]-suba[i-1]
}

subaSinUnos<-suba2>1
suba2.1<-suba2[subaSinUnos]
suba3<-suba2.1[1:71]

#Hago un fit para mi distribucion y hallo mi rate car
fit.suba<-fitdist(suba3, "gamma")
shape.suba<-fit.suba$estimate[1]
rate.suba<-fit.suba$estimate[2]



##
#Estacion tunal
##
tunal <- vector(mode="numeric")
cont=0
tunal2<-vector(mode="numeric")
cont2=0

#Hallo el vector de tiempos de las llegadas
for(i in 1:length(matdata2.0[,"FECHA"])){
  datoActual=as.numeric(matdata2.0[i,11])
  if(datoActual==1){
    cont=cont+1
    fechaActual=matdata2.0[i,1]
    tunal[cont]=fechaActual
  }
}
#Hallo el vector de tiempos entre llegadas
for(i in 2:length(tunal)){
  cont2=cont2+1
  tunal2[cont2]=tunal[i]-tunal[i-1]
}

tunalSinUnos<-tunal2>1
tunal3<-tunal2[tunalSinUnos]

#Hago un fit para mi distribucion y hallo mi rate tunal
fit.tunal<-fitdist(tunal3, "gamma")
fit.tunal
shape.tunal<-fit.tunal$estimate[1]
rate.tunal<-fit.tunal$estimate[2]


##
#Estacion usaquen
##
usaquen <- vector(mode="numeric")
cont=0
usaquen2<-vector(mode="numeric")
cont2=0

#Hallo el vector de tiempos de las llegadas
for(i in 1:length(matdata2.0[,"FECHA"])){
  datoActual=as.numeric(matdata2.0[i,12])
  if(datoActual==1){
    cont=cont+1
    fechaActual=matdata2.0[i,1]
    usaquen[cont]=fechaActual
  }
}
#Hallo el vector de tiempos entre llegadas
for(i in 2:length(usaquen)){
  cont2=cont2+1
  usaquen2[cont2]=usaquen[i]-usaquen[i-1]
}

usaquenSinUnos<-usaquen2>1
usaquen3<-usaquen2[usaquenSinUnos]

#
#Hago un fit para mi distribucion y hallo mi rate usaquen
fit.usaquen<-fitdistr(usaquen3,"Exponential")
rate.usaquen<-fit.usaquen$estimate
#1.
#Realizo un histograma y una grafica exponencial con mi rate
hist(usaquen3, freq = FALSE)
curve(dexp(x, rate = rate.usaquen), col = "red", add = TRUE)
#2.
#Hago el test KS para saber si se distribuye exponencial
ks.test(usaquen3,"pexp",rate.usaquen)
#3.
#Con un QQ Plot puedo revisar si se comporta como una exponencial
qqplot(x=qexp(ppoints(1000)), y=usaquen3, main="Exponential Q-Q Plot Usaquen",
       xlab="Theoretical Quantiles", ylab= "Tiempo entre llegadas Quantiles")
qqline(usaquen3, distribution=qexp)


###MEZCLA DE TODAS LAS VARIABLES
mezcla<-c(carvajal3,car3,guaymaral3,Kennedy3,lasferias3,minambiente3,puentearanda3,sancristobal3,suba3,tunal3,usaquen3)
#Hago un fit para mi distribucion y hallo mi rate mezcla
fit.mezcla<-fitdist(mezcla, "gamma")
fit.mezcla
shape.mezcla<-fit.mezcla$estimate[1]
rate.tunal<-fit.mezcla$estimate[2]
#Un QQ Plot y un Culley&Frey que me muestra que tan acorde esta la mezcla de mis variables al estimado
plot(fit.mezcla)
descdist(mezcla,discrete = FALSE)


#Hallo mis probabilidades p y q para cada estacion
#EXPONENCIALES
#1 Carvajal

q.carvajal<-pexp(1,rate.carvajal)
pexp(1,rate.carvajal*24)

#2 Las Ferias
q.lasferias<-pexp(1,rate.lasferias)
#3 MinAmbiente
q.minambiente<-pexp(1,rate.minambiente)
#4 Puente Aranda
q.puenteAranda<-pexp(1,rate.puentearanda)
#5 San Cristobal
q.sancristobal<-pexp(1,rate.sancristobal)
#6 Usaquen
q.usaquen<-pexp(1,rate.usaquen)

pexp(4.02,rate.usaquen)

#GAMMA
#1 CAR
q.car<-pgamma(1,shape.car,rate.car)
#2 Guaymaral
q.guaymaral<-pgamma(1,shape.guaymaral,rate.guaymaral)
#3 Kennedy
q.kennedy<-pgamma(1,shape.kennedy,rate.kennedy)
#4 Suba
q.suba<-pgamma(1,shape.suba,rate.suba)
#5 Tunal
q.tunal<-pgamma(1,shape.tunal,rate.tunal)


##VERSION 2
#Hallo mis probabilidades p y q para cada estacion
#EXPONENCIALES
#1 Carvajal

q.carvajal<-pexp(1,rate.carvajal*24)
#2 Las Ferias
q.lasferias<-pexp(1,rate.lasferias*24)
#3 MinAmbiente
q.minambiente<-pexp(1,rate.minambiente*24)
#4 Puente Aranda
q.puenteAranda<-pexp(1,rate.puentearanda*24)
#5 San Cristobal
q.sancristobal<-pexp(1,rate.sancristobal*24)
#6 Usaquen
q.usaquen<-pexp(1,rate.usaquen*24)

#GAMMA
#1 CAR
q.car<-pgamma(1,shape.car,rate.car*24)
#2 Guaymaral
q.guaymaral<-pgamma(1,shape.guaymaral,rate.guaymaral*24)
#3 Kennedy
q.kennedy<-pgamma(1,shape.kennedy,rate.kennedy*24)
#4 Suba
q.suba<-pgamma(1,shape.suba,rate.suba*24)
#5 Tunal
q.tunal<-pgamma(1,shape.tunal,rate.tunal*24)

Resultados<-array(c(q.carvajal,q.lasferias,q.minambiente,q.puenteAranda,q.sancristobal,q.usaquen,q.car,q.guaymaral,q.kennedy,q.suba,q.tunal))
Resultados

#1 Carvajal

q2.carvajal<-pexp(1,rate.carvajal*24*0.7)
#2 Las Ferias
q2.lasferias<-pexp(1,rate.lasferias*24*0.7)
#3 MinAmbiente
q2.minambiente<-pexp(1,rate.minambiente*24*0.7)
#4 Puente Aranda
q2.puenteAranda<-pexp(1,rate.puentearanda*24*0.7)
#5 San Cristobal
q2.sancristobal<-pexp(1,rate.sancristobal*24*0.7)
#6 Usaquen
q2.usaquen<-pexp(1,rate.usaquen*24*0.7)

#GAMMA
#1 CAR
q2.car<-pgamma(1,shape.car,rate.car*24*0.7)
#2 Guaymaral
q2.guaymaral<-pgamma(1,shape.guaymaral,rate.guaymaral*24*0.7)
#3 Kennedy
q2.kennedy<-pgamma(1,shape.kennedy,rate.kennedy*24*0.7)
#4 Suba
q2.suba<-pgamma(1,shape.suba,rate.suba*24*0.7)
#5 Tunal
q2.tunal<-pgamma(1,shape.tunal,rate.tunal*24*0.7)

Resultados2<-array(c(q2.carvajal,q2.lasferias,q2.minambiente,q2.puenteAranda,q2.sancristobal,q2.usaquen,q2.car,q2.guaymaral,q2.kennedy,q2.suba,q2.tunal))
Resultados2


