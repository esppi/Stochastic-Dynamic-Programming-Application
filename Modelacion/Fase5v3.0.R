####CODIGO DE LA HERRAMIENTA GENERADA PARA LA SDA PARA PROGRAMACION DE MANTENIMIENTOS PARA EL MES DE ENERO
####UNIVERSIDAD DE LOS ANDES######
###MODELOS PROBABILISTICOS######
###David Espinel - Daniel Montoya - Camilo Morales - Juan Esteban Giraldo #######
###Por favor dirijirse a Session<-Set Working Directory<-To Source File Location
###Este R Script pretende mostrar el razonamimento y operaciones matriciales detras del calculo de la funcion recursiva realizado en la aplicacion de Shiny

#Librerias
library(ggplot2)
library(plotly)


###Leo los datos con un if dependiendo del input estacion
Iniciales<-read.csv("Iniciales.csv")
inputEstacion<-"Guaymaral"
if(inputEstacion=="Guaymaral"){
  data<-read.csv("RecursivaGuaymaral.csv")
  mantenimientoInicial<-Iniciales[1,2]
}else if(inputEstacion=="Suba"){
  data<-read.csv("RecursivaSuba.csv")
  mantenimientoInicial<-Iniciales[2,2]
  
}else if(inputEstacion=="Usaquen"){
  data<-read.csv("RecursivaUsaquen.csv")
  mantenimientoInicial<-Iniciales[3,2]
  
}else if(inputEstacion=="Las Ferias"){
  data<-read.csv("RecursivaLasFerias.csv")
  mantenimientoInicial<-Iniciales[4,2]
  
}else if(inputEstacion=="Fontibon"){
  data<-read.csv("RecursivaFontibon.csv")
  mantenimientoInicial<-Iniciales[5,2]
  
}else if(inputEstacion=="Bolivia"){
  data<-read.csv("RecursivaBolivia.csv")
  mantenimientoInicial<-Iniciales[6,2]
  
}else if(inputEstacion=="IDRD"){
  data<-read.csv("RecursivaIDRD.csv")
  mantenimientoInicial<-Iniciales[7,2]
  
}else if(inputEstacion=="Puente Aranda"){
  data<-read.csv("RecursivaPuenteAranda.csv")
  mantenimientoInicial<-Iniciales[8,2]
  
}else if(inputEstacion=="Kennedy"){
  data<-read.csv("RecursivaKennedy.csv")
  mantenimientoInicial<-Iniciales[9,2]
  
}else if(inputEstacion=="Carvajal"){
  data<-read.csv("RecursivaCarvajal.csv")
  mantenimientoInicial<-Iniciales[10,2]
  
}else if(inputEstacion=="Tunal"){
  data<-read.csv("RecursivaTunal.csv")
  mantenimientoInicial<-Iniciales[11,2]
  
}else if(inputEstacion=="San Cristobal"){
  data<-read.csv("RecursivaSanCristobal.csv")
  mantenimientoInicial<-Iniciales[12,2]
  
}else if(inputEstacion=="Sagrado Corazon"){
  data<-read.csv("Sagrado Corazon.csv")
  mantenimientoInicial<-Iniciales[13,2]
  
}


matData<-data.matrix(data)
###Guardo las 3 matrices
###Costos inmediatos
CI<-matData[1:2,]
###Probabilidades de transicion si reparo
PR<-matData[3:4,]
###Probabilidades de transicion si no reparo
PNR<-matData[5:6,]


######INICIO CREACION DE TABLA CON DATOS INPUT ####################

#Creo la lista de nombres
#1. Lista de nombres de MF
MFS<-list()

#2. Lista de nombres de Ms
MS<-list()

###EN ESTE CASO MI INPUT ES EL COSTO DIA SIN DATOS afecta FILA 1 de CI,mantenimiento afecta columna 1
###Este valor inicial debe venir de un archivo
inputMantenimiento<-400
margenMantenimiento<-inputMantenimiento-mantenimientoInicial

###Inicializo
diaSinDatosInicial<-CI[1,2]
###Este es el input
inputDiaSinDatos<-700
margenDiaSinDatos<-inputDiaSinDatos-diaSinDatosInicial
CITabla<-CI

###Genero el cambio en la tabla de CITabla
##Cambios por sin datos
CITabla[1,1]<-CITabla[1,1]+margenDiaSinDatos
CITabla[1,2]<-CITabla[1,2]+margenDiaSinDatos
##Cambios por mantenimiento
CITabla[1,1]<-CITabla[1,1]+margenMantenimiento
CITabla[2,1]<-CITabla[1,1]+margenMantenimiento


###Con este loop hayo que debo hacer cada epoca dependiendo de mi estado, y el mejor camino
for(i in 30:1){
  #Defino mi f_30
  if(i==30){
    MFS[[i]]<-assign(
      paste0("MF",i,"")
      ,
      rbind(
        #Fila 1, de mis valores minimos en costos inmediatos
        cbind(min(CITabla[1,]),min(CITabla[2,]))
        ,
        #Fila 2, de mi indice del minimo, 1 si es Reparar, 2 si es no Reparar
        cbind(which.min(CITabla[1,]),which.min(CITabla[2,]))
      )
    )
  }else{
    #De aca en adelante son f_29 hasta f_1
    #1. La matriz M de cada i
    
    MS[[i]]<-assign(
      paste0("M",i,"")
      ,
      cbind(
        #Columna 1
        CITabla[,1]
        +rowSums(
          rbind(
            MFS[[i+1]][1,],MFS[[i+1]][1,]
          )*PR
        )
        ,
        #Columna 2
        CITabla[,2]
        +rowSums(
          rbind(
            MFS[[i+1]][1,],MFS[[i+1]][1,]
          )*PNR
        )
      )
    )
    #2. La matriz MF de cada i de 29 a 1
    MFS[[i]]<-assign(
      paste0("MF",i,"")
      ,
      rbind(
        #Fila 1, de mis valores minimos en costos inmediatos
        cbind(min(MS[[i]][1,]),min(MS[[i]][2,]))
        ,
        #Fila 2, de mi indice del minimo, 1 si es Reparar, 2 si es no Reparar
        cbind(which.min(MS[[i]][1,]),which.min(MS[[i]][2,])) 
      )
    )
  }
}

###Ya con la politica que debo seguir proceso a imprimirla de manera amena
###Asumiendo que mi estacion esta funcionando
MFS[1]
ResultadosDanado<-array(0,dim=c(2,30),dimnames = list(c("Costo","Decision"),c(1:30)))
ResultadosFuncionando<-array(0,dim=c(2,30),dimnames = list(c("Costo","Decision"),c(1:30)))
####CAMBIAR LOS RESULTADOS FUNCIONANDO; PARA CADA OBJETO MF, la columna 1 es estado 0, con el valor y la desicion que tomo

for(i in 30:1){
  #Costo
  ResultadosDanado[1,i]<-MFS[[i]][1,1]
  #Decision
  ResultadosDanado[2,i]<-if(MFS[[i]][2,1]==1){"R"}else{"NR"}
  
}
for(i in 30:1){
  #Costo
  ResultadosFuncionando[1,i]<-MFS[[i]][1,2]
  #Decision
  ResultadosFuncionando[2,i]<-if(MFS[[i]][2,2]==1){"R"}else{"NR"}
}
write.csv(ResultadosDanado,file = "")
write.csv(ResultadosFuncionando,file = "")


####CREAR VISUALIZACION TABLA
##ACA
#######################3 FIN CREACION DE TABLA CON DATOS INPUT #######################3

############### INICIO DE VISUALIZACION COSTO DIA SIN DATOS ##################
#Creo la lista de nombres
#1. Lista de nombres de MF
MFS<-list()
#2. Lista de nombres de Ms
MS<-list()
#####EL FACTOR VA A SER EL COSTO DE UN DIA SIN DATOS
##Donde empieza
ExtraC<--800
##Incrementos
Extra<-100
Contador<-0
Costos<-list()
CI2<-CITabla

while(ExtraC<301){
  ###Añado el Extra
  ExtraC<-ExtraC+Extra
  Contador<-Contador+1
  CI2[1,1]<-CITabla[1,1]+ExtraC
  CI2[1,2]<-CITabla[1,2]+ExtraC
  
  
  ###Con este loop hayo que debo hacer cada epoca dependiendo de mi estado, y el mejor camino
  for(i in 30:1){
    #Defino mi f_30
    if(i==30){
      MFS[[i]]<-assign(
        paste0("MF",i,"")
        ,
        rbind(
          #Fila 1, de mis valores minimos en costos inmediatos
          cbind(min(CI2[1,]),min(CI2[2,]))
          ,
          #Fila 2, de mi indice del minimo, 1 si es Reparar, 2 si es no Reparar
          cbind(which.min(CI2[1,]),which.min(CI2[2,]))
        )
      )
    }else{
      #De aca en adelante son f_29 hasta f_1
      #1. La matriz M de cada i
      
      MS[[i]]<-assign(
        paste0("M",i,"")
        ,
        cbind(
          #Columna 1
          CI2[,1]
          +rowSums(
            rbind(
              MFS[[i+1]][1,],MFS[[i+1]][1,]
            )*PR
          )
          ,
          #Columna 2
          CI2[,2]
          +rowSums(
            rbind(
              MFS[[i+1]][1,],MFS[[i+1]][1,]
            )*PNR
          )
        )
      )
      #2. La matriz MF de cada i de 29 a 1
      MFS[[i]]<-assign(
        paste0("MF",i,"")
        ,
        rbind(
          #Fila 1, de mis valores minimos en costos inmediatos
          cbind(min(MS[[i]][1,]),min(MS[[i]][2,]))
          ,
          #Fila 2, de mi indice del minimo, 1 si es Reparar, 2 si es no Reparar
          cbind(which.min(MS[[i]][1,]),which.min(MS[[i]][2,])) 
        )
      )
    }
  }
  ######LLENO LA LISTA DE VALORES JUNTO CON SU EXTRA DE MANTENIMIENTO
  index<-which.min(MFS[[1]][1,])
  minimo<-min(MFS[[1]][1,])
  Costos[[Contador]]<-array(c(minimo,ExtraC),dim = c(2,1))
}

###Transformo mis costos en una matriz graficable
matCostos<-array(0,dim = c(2,length(Costos)),dimnames = list(c("Costo (y)","Extra (x)"),c(1:length(Costos))))
for(i in 1:length(Costos)){
  #Costo
  matCostos[1,i]<-Costos[[i]][1]
  #Extra
  matCostos[2,i]<-Costos[[i]][2]
}

matCostos

##Realizo la grafica
p<-qplot(matCostos[2,]+CITabla[1,2],matCostos[1,],xlab="Costo de un día sin datos [Estacion ]",ylab="Costo de funcion recursiva en la epoca 1 ($)")+geom_line()+geom_label(aes(label=round(matCostos[1,],digits=3),vjust=-0.15))+scale_x_continuous(breaks = seq(from=-600, to=200000,by=100))+scale_y_continuous(breaks = seq(from=0, to=1000000,by=2000))

p

plot1<-plot_ly(type="scatter",mode="markers",x=matCostos[2,]+CITabla[1,2],y=matCostos[1,]) %>%
  layout(xaxis=list(title="Costo de un dia sin datos"),yaxis=list(title="Costo de la funcion recursiva en la epoca 2 ($)"),showlegend=FALSE)
plot1


################# FIN VISUALIZACION DIA SIN DATOS #################################
ResultadosFuncionando

################INICIO VISUALIZACION MANTENIMIENTO ###################################3
#Creo la lista de nombres
#1. Lista de nombres de MF
MFS2<-list()
#2. Lista de nombres de Ms
MS2<-list()

#####EL FACTOR VA A SER EL COSTO DE UN DIA SIN DATOS
##Donde empieza
Extra2C2<--500
##Incrementos
Extra2<-100
Contador2<-0
Costos2<-list()
CI3<-CITabla

while(Extra2C2<301){
  ###Añado el Extra2
  Extra2C2<-Extra2C2+Extra2
  Contador2<-Contador2+1
  CI3[1,1]<-CITabla[1,1]+Extra2C2
  CI3[2,1]<-CITabla[2,1]+Extra2C2
  
  
  ###Con este loop hayo que debo hacer cada epoca dependiendo de mi estado, y el mejor camino
  for(i in 30:1){
    #Defino mi f_30
    if(i==30){
      MFS2[[i]]<-assign(
        paste0("MF",i,"")
        ,
        rbind(
          #Fila 1, de mis valores minimo2s en Costos2 inmediatos
          cbind(min(CI3[1,]),min(CI3[2,]))
          ,
          #Fila 2, de mi indice del minimo2, 1 si es Reparar, 2 si es no Reparar
          cbind(which.min(CI3[1,]),which.min(CI3[2,]))
        )
      )
    }else{
      #De aca en adelante son f_29 hasta f_1
      #1. La matriz M de cada i
      
      MS2[[i]]<-assign(
        paste0("M",i,"")
        ,
        cbind(
          #Columna 1
          CI3[,1]
          +rowSums(
            rbind(
              MFS2[[i+1]][1,],MFS2[[i+1]][1,]
            )*PR
          )
          ,
          #Columna 2
          CI3[,2]
          +rowSums(
            rbind(
              MFS2[[i+1]][1,],MFS2[[i+1]][1,]
            )*PNR
          )
        )
      )
      #2. La matriz MF de cada i de 29 a 1
      MFS2[[i]]<-assign(
        paste0("MF",i,"")
        ,
        rbind(
          #Fila 1, de mis valores minimo2s en Costos2 inmediatos
          cbind(min(MS2[[i]][1,]),min(MS2[[i]][2,]))
          ,
          #Fila 2, de mi indice del minimo2, 1 si es Reparar, 2 si es no Reparar
          cbind(which.min(MS2[[i]][1,]),which.min(MS2[[i]][2,])) 
        )
      )
    }
  }
  ######LLENO LA LISTA DE VALORES JUNTO CON SU Extra2 DE MANTENIMIENTO
  index2<-which.min(MFS2[[1]][1,])
  minimo2<-min(MFS2[[1]][1,])
  Costos2[[Contador2]]<-array(c(minimo2,Extra2C2),dim = c(2,1))
}

###Transformo mis Costos2 en una matriz graficable
matCostos2<-array(0,dim = c(2,length(Costos2)),dimnames = list(c("Costo (y)","Extra2 (x)"),c(1:length(Costos2))))
for(i in 1:length(Costos2)){
  #Costo
  matCostos2[1,i]<-Costos2[[i]][1]
  #Extra2
  matCostos2[2,i]<-Costos2[[i]][2]
}

matCostos2

##Realizo la grafica
p2<-qplot(matCostos2[2,]+mantenimientoInicial+margenMantenimiento,matCostos2[1,],xlab="Costo de mantenimiento [Estacion ]",ylab="Costo de funcion recursiva en la epoca 1 ($)")+geom_line()+geom_label(aes(label=round(matCostos2[1,],digits=3),vjust=-0.15))+scale_x_continuous(breaks = seq(from=-600, to=1200000,by=100))+scale_y_continuous(breaks = seq(from=0, to=19000000,by=2000))
p2

ResultadosDanado
ResultadosFuncionando

library(DT)
DT::datatable(ResultadosDanado)

datatable(head(ResultadosDanado), options = list(dom = 't'), filter = list(position = "false"))


library(plotly)


plot2<-plot_ly(type="scatter",mode="markers",x=matCostos2[2,]+mantenimientoInicial+margenMantenimiento,y=matCostos2[1,]) %>%
  layout(xaxis=list(title="Costo de mantenimiento"),yaxis=list(title="Costo de la funcion recursiva en la epoca 2 ($)"),showlegend=FALSE)
plot2

