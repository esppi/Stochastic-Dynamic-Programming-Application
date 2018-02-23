# Entrega Final Modelos Probabilisticos
# Herramienta Shiny
# David Espinel - 201531260
# Daniel Montoya - Juan Esteban Giraldo - Camilo Morales
# Por favor dirigirse a Session<-Set Working Directory<-To source file location
# Dar click al boton Run App

#Librerias necesarias

#Desarrollo aplicacion
library(shiny)
#Creacion de graficas
library(plotly)
#Despliegue on line
library(rsconnect)
#Visualizacion tablas
library(DT)

# Define la interfaz de la app
interfaz <- fluidPage(
  
  #Titulo
  title="Planeadora de Mantenimientos",
  tags$hr(style="border-color: green;"),
  #Primer panel con los logotipos
  fluidRow(
    # Application title
  column(1,img(src="alcaldia.jpg",width=110*2.72,height=110)),
  column(1,offset=4,img(src="rstudio.png",height=110,width=2.85*110)),
  column(1,offset=3,img(src="logo-uniandes.jpg",height=110,width=3.16*110))
    
  ),
  
  tags$hr(style="border-color: green;"),
  
  titlePanel(title="Política óptima de mantenimientos para las estaciones de la Red de Monitoreo de Calidad del Aire de Bogota (RMCAB)."),
  
  tags$hr(style="border-color: green;"),
  #Panel de introducción
  fluidRow(align="center",h4("Introducción")),
  fluidRow(
    column(10, offset=1,p("La aplicación web planeadora de mantenimientos nace como solución a los problemas presentados en la secretaria de ambiente en Bogotá. Diferentes estaciones de captación de información de la secretaria presentan fallas en sus equipos lo que inevitablemente las saca de operación durante algún tiempo. Cuando esto ocurre la secretaria es incapaz de obtener información de la estación comprometida."), p("Planeadora de mantenimientos permitirá a la administración de la secretaria planear con antelación los mantenimientos que se deben realizar en las estaciones para reducir al máximo la posibilidad de fallas y colapsos. Basada en modelos estadísticos robustos, la aplicación ofrecerá la mejor decisión posible a tomar para un horizonte de tiempo de 31 días (Enero 2018). Es decir, una vez seleccionados los parámetros requeridos Planeadora de mantenimientos encontrara la decisión más acertada para la gerencia para cada uno de los siguientes 31 días."))
  ),
  
  
  tags$hr(style="border-color: green;"),
  
  
  #Panel de parametros, es decir, inputs
  fluidRow(align="center",h4("Parámetros")),
  
  p(),
  
  fluidRow(
    column(3,
           selectInput("inputEstacionui","Estación:",c("Guaymaral","Suba","Usaquen","Las Ferias","IDRD","Puente Aranda","Kennedy","Carvajal","Tunal","San Cristobal"))
           
           ),
    column(4, offset = 1,
           numericInput("inputMantenimientoui","Costo del mantenimiento:",400,0,50000,100)
    ),
    column(4,
           numericInput("inputDiaSinDatosui","Costo de un día sin datos:",700,0,50000,100)
    )
  ),
  
      p(),
      tags$hr(style="border-color: green;"),
      #Panel de resultados
      fluidRow(align="center",h4("Resultados")),
      fluidRow(
        column(10, offset=1,p("A continuación se mostrarán las tablas de decisión para cada estado en el que puede estar la estación, horizontalmente se encuentra el día y verticalemente el costo y decisión que se debe tomar, donde R es reparar y NR es no reparar. Estos valores, al igual que las gráficas, cambian dependiendo de los parámetros ingresados. Los costos estan representados en miles de COP."))
                             ),
  
  
      tags$hr(style="border-color: green;"),
  
    # Muestra la tabla para el estado funcionando
      strong("Si no está funcionando:")
      ,
      DT::dataTableOutput("danado")
      ,
  #tags$audio(src = "sound.mp3", type = "audio/mp3", autoplay = NA, controls = NA)
      tags$hr(),
      strong("Si está funcionando:")
      ,
      DT::dataTableOutput("funcionando")
      ,
      tags$hr(),
    ##Panel de grafica 1
      fluidRow(align="center",h4("Gráfica 1")),
      fluidRow(
        column(10, offset=1,p("La gráfica 1 representa un análisis de sensibilidad del comportamiento del costo mínimo (función recursiva en Época 1) para enero, dado que se genera un cambio en el costo de mantenimiento de la estación y asumiendo que se empieza en estado funcional. En pocas palabras, permite ver cómo se comporta el costo total con cambios en el costo de mantenimiento."))
      ),
  
      plotlyOutput("plot2")
      ,
      tags$hr(),
  
      fluidRow(align="center",h4("Gráfica 2")),
  ##Panel de grafica 2
      fluidRow(
      column(10, offset=1,p("La gráfica 2 representa un análisis de sensibilidad del comportamiento del costo mínimo (función recursiva en Época 1) para enero, dado que se genera un cambio en el costo de un día sin datos en la estación y asumiendo que se empieza en estado funcional. En pocas palabras, permite ver cómo se comporta el costo total con cambios en el costo de mantenimiento."))
      ),
  
      #Aca va plot 1
      plotlyOutput("plot1")
      ,
      tags$hr()
      ,
      p("Esta herramienta fue diseñada para la entrega final del curso Modelos Probabilísticos de la Universidad de los Andes.")
      ,
      em("Por: David Espinel - Juan Esteban Giraldo - Daniel Montoya - Camilo Morales")
      ,
      p(),
      em("En caso de dudas contactar al correo: esppinel@gmail.com")
      ,
      p(),
      em("Los datos se obtuvieron de la Secretaria de Ambiente (SDA).")
  
)

# Define la logica para graficar el chart
servidor <- function(input, output) {
  
  output$danado <- DT::renderDataTable({
    ### INICIO TABLAS ###
    ###Leo los datos con un if dependiendo del input estacion
    Iniciales<-read.csv("Iniciales.csv")
    inputEstacion<-input$inputEstacionui
    #inputEstacion<-"Guaymaral"
    
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
    inputMantenimiento<-input$inputMantenimientoui
    margenMantenimiento<-inputMantenimiento-mantenimientoInicial
    
    ###Inicializo
    diaSinDatosInicial<-CI[1,2]
    ###Este es el input
    inputDiaSinDatos<-input$inputDiaSinDatosui
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
    for(i in 31:1){
      #Defino mi f_31, solo viene de los costos inmediatos
      if(i==31){
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
        #De aca en adelante son f_29 hasta f_1, con el costo inmediato y el futuro
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
    ResultadosDanado<-array(0,dim=c(2,31),dimnames = list(c("Costo ($)","Decisión"),c(1:31)))
    ResultadosFuncionando<-array(0,dim=c(2,31),dimnames = list(c("Costo ($)","Decisión"),c(1:31)))
    ####CAMBIAR LOS RESULTADOS FUNCIONANDO; PARA CADA OBJETO MF, la columna 1 es estado 0, con el valor y la desicion que tomo
    
    for(i in 31:1){
      #Costo
      ResultadosDanado[1,i]<-round(MFS[[i]][1,1],1)
      #Decision
      ResultadosDanado[2,i]<-if(MFS[[i]][2,1]==1){"R"}else{"NR"}
      
    }

    ##### FIN TABLAS ########
    datatable(ResultadosDanado,options = list(dom="t"))
  })
  
  output$funcionando <- DT::renderDataTable({
    ### INICIO TABLAS ###
    ###Leo los datos con un if dependiendo del input estacion
    Iniciales<-read.csv("Iniciales.csv")
    inputEstacion<-input$inputEstacionui
    #inputEstacion<-"Guaymaral"
    
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
    inputMantenimiento<-input$inputMantenimientoui
    margenMantenimiento<-inputMantenimiento-mantenimientoInicial
    
    ###Inicializo
    diaSinDatosInicial<-CI[1,2]
    ###Este es el input
    inputDiaSinDatos<-input$inputDiaSinDatosui
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
    for(i in 31:1){
      #Defino mi f_31
      if(i==31){
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
        #De aca en adelante son f_31 hasta f_1
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
    ResultadosDanado<-array(0,dim=c(2,31),dimnames = list(c("Costo ($)","Decisión"),c(1:31)))
    ResultadosFuncionando<-array(0,dim=c(2,31),dimnames = list(c("Costo ($)","Decisión"),c(1:31)))
    ####CAMBIAR LOS RESULTADOS FUNCIONANDO; PARA CADA OBJETO MF, la columna 1 es estado 0, con el valor y la desicion que tomo

    for(i in 31:1){
      #Costo
      ResultadosFuncionando[1,i]<-round(MFS[[i]][1,2],1)
      #Decision
      ResultadosFuncionando[2,i]<-if(MFS[[i]][2,2]==1){"R"}else{"NR"}
    }
    ##### FIN TABLAS ########
    datatable(ResultadosFuncionando,options=list(dom="t"))
    })
  
  output$plot1<-renderPlotly({
    ### INICIO TABLAS ###
    ###Leo los datos con un if dependiendo del input estacion
    Iniciales<-read.csv("Iniciales.csv")
    inputEstacion<-input$inputEstacionui
    #inputEstacion<-"Guaymaral"
    
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
    inputMantenimiento<-input$inputMantenimientoui
    #inputMantenimiento<-400
    margenMantenimiento<-inputMantenimiento-mantenimientoInicial
    ###Inicializo
    diaSinDatosInicial<-CI[1,2]
    ###Este es el input
    inputDiaSinDatos<-input$inputDiaSinDatosui
    #inputDiaSinDatos<-700
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
    for(i in 31:1){
      #Defino mi f_31, solamente con los costos inmediatos
      if(i==31){
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
      ###Anado el Extra
      ExtraC<-ExtraC+Extra
      Contador<-Contador+1
      CI2[1,1]<-CITabla[1,1]+ExtraC
      CI2[1,2]<-CITabla[1,2]+ExtraC
      
      
      ###Con este loop hayo que debo hacer cada epoca dependiendo de mi estado, y el mejor camino
      for(i in 31:1){
        #Defino mi f_31
        if(i==31){
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
    
    ##Realizo la grafica
    a <- list(
      x=inputDiaSinDatos,
      y =matCostos[1,8],
      text = "Valor inicial",
      xref = "x",
      yref = "y",
      showarrow = TRUE,
      arrowhead = 7,
      ax = 20,
      ay = -40
    )
    
    plot_ly(type="scatter",mode="markers",x=matCostos[2,]+CITabla[1,2],y=matCostos[1,], line=list(color='rgba(34, 139, 34, .8)'),
            marker = list(size = 7,color = 'rgba(100, 130, 100, .9)',line = list(color = 'rgba(34, 139, 34, .8)',width = 2))) %>%
      layout(annotations=a,xaxis=list(title="Costo de un día sin datos ($)",zeroline=FALSE),yaxis=list(title="Costo para enero (función recursiva en Época 1) ($)",zeroline=FALSE))
    
  })

  output$plot2<-renderPlotly({
    ### INICIO TABLAS ###
    ###Leo los datos con un if dependiendo del input estacion
    Iniciales<-read.csv("Iniciales.csv")
    inputEstacion<-input$inputEstacionui
    #inputEstacion<-"Guaymaral"
    
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
    inputMantenimiento<-input$inputMantenimientoui
    #inputMantenimiento<-400
    margenMantenimiento<-inputMantenimiento-mantenimientoInicial
    ###Inicializo
    diaSinDatosInicial<-CI[1,2]
    ###Este es el input
    inputDiaSinDatos<-input$inputDiaSinDatosui
    #inputDiaSinDatos<-700
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
    for(i in 31:1){
      #Defino mi f_31
      if(i==31){
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
      ###ANado el Extra2
      Extra2C2<-Extra2C2+Extra2
      Contador2<-Contador2+1
      CI3[1,1]<-CITabla[1,1]+Extra2C2
      CI3[2,1]<-CITabla[2,1]+Extra2C2
      ###Con este loop hayo que debo hacer cada epoca dependiendo de mi estado, y el mejor camino
      for(i in 31:1){
        #Defino mi f_31
        if(i==31){
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

    b <- list(
      x=inputMantenimiento,
      y =matCostos2[1,5],
      text = "Valor inicial",
      xref = "x",
      yref = "y",
      showarrow = TRUE,
      arrowhead = 7,
      ax = 20,
      ay = -40
    )
    ##Realizo la grafica
    plot_ly(type="scatter",mode="markers",x=matCostos2[2,]+mantenimientoInicial+margenMantenimiento,y=matCostos2[1,],line=list(color='rgba(34, 139, 34, .8)'),
            marker = list(size = 7,color = 'rgba(100, 130, 100, .9)',line = list(color = 'rgba(34, 139, 34, .8)',width = 2))) %>%
      layout(annotations=b,xaxis=list(title="Costo de mantenimiento ($)"),yaxis=list(title="Costo para enero (función recursiva en Época 1) ($)"),showlegend=FALSE)
    })
  
}


# Run the application 
shinyApp(ui = interfaz, server = servidor)