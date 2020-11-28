#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
installed.packages("shinythemes")
library(shiny)
library(shinyjs)
library(readxl)
library(RMySQL)
library(DBI)
library(dplyr)
library (ggplot2)
library(shinythemes)

source("www/recoleccion.R")
source("www/introduccion.R")
source("www/preprocesamiento.R")
source("www/preprocesamiento2.R")
source("www/conexionMySQL.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("cyborg"),
  navbarPage( "Proyecto final sobre los casos d e violencia en el Peru",
              tabPanel("Presentacion",introduccion),
              tabPanel("Recoleccion",
                       recoleccion )
              
              ,tabPanel("Preprocesamiento",preprocesamiento1
                       
              )
              ,tabPanel("Preprocesamiento",preprocesamiento2
                        
              )
              ,tabPanel("Subir XLSX a BD" ,h3("Conexion con la base de datos"),verbatimTextOutput("MySQL en AWS"),h3("Script "), hr() ,verbatimTextOutput("ConexionBaseD") )
              
              ,tabPanel("Modelado e implementacion BD" ,h3("Imputacion de tablas a la base de datos"),verbatimTextOutput("MySQL"),h3("Script "), hr() ,verbatimTextOutput("CreacionTablas") )
              
              ,tabPanel('Consultas SQL',
                        navlistPanel(
                         tabPanel("Consultas con dbGetSQL", h4("SQL"),hr(),selectInput("selectsq", label = NULL, choices = c('consulta 1'='1','consulta 2'='2','consulta 3'='3','consulta 4'='4','consulta 5'='5','consulta 6'='6','consulta 7'='7','consulta 8'='8','consulta 9'='9','consulta 10'='10')),checkboxInput("control3", "Mostrar Codigo", FALSE),hr(),verbatimTextOutput("consulta3"),dataTableOutput("tablaS4"))
                          
                        )
              )
              
              ,tabPanel('Graficos',
                        navlistPanel( 
                          tabPanel("Graficos GGPLOT2",h4("GGPLOT2"),hr(),selectInput("selectgg", label = NULL, choices = c('consulta 1'='1','consulta 2'='2','consulta 3'='3','consulta 4'='4','consulta 5'='5','consulta 6'='6','consulta 7'='7','consulta 8'='8','consulta 9'='9','consulta 10'='10')),checkboxInput("control4", "Mostrar Codigo", FALSE),hr(),verbatimTextOutput("consulta4"),plotOutput('plot1'))))
              
              ,tabPanel('Modelo',
                        navlistPanel(  
                          tabPanel("Regresion Lineal",hr(),checkboxInput("check", "Mostrar Codigo", FALSE),verbatimTextOutput("CodRL"),hr(), plotOutput('plotRL') , sliderInput("xRL" ,"Porcentaje de adolescentes madres primerizas y profesionales en partos", min = 1 , max = 100, value = 1 ), sidebarLayout(sidebarPanel(h5("Predicion del porcentaje de jovenes madres primerizas que posiblemente fueron atendidas por partistas profesionales:")),mainPanel(textOutput("IdPrediccionRL")))  )
                          ,tabPanel("KNN",h4("Codigo implementacion KNN"),verbatimTextOutput("CodKnn"),sliderInput("xKnn" ,"Porcentaje de adolescentes madres primerizas", min = 1 , max = 100, value = 1 ),sliderInput("yKnn" ,"Porcentaje profesionales realizando partos", min = 1 , max = 100, value = 1 ),sliderInput("kKnn" ,"valor de k", min = 1 , max = 7, value = 1 ) , tableOutput("tablaKNN"))
                        ) )
              ,tabPanel("Exportar",actionButton("file20",  "Seleccione carpeta donde guardar CSV Adolescente y Profesional")
                        
              )
              
  )
  
  
  
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  getwd()

  
 # tablaS1 <- input$file20
#  archivo<-file.choose()
#  write.csv(qdivision_profesionales_dep, file = "porcentaje_adolescentes.csv")

  
  
  
  ########4.Transformacion y consultas exploratorias##########
  
  
  
  
  #1. Seleccionar elementos de tabla departamentos
  qdepartamentos<-dbGetQuery(conexion,"select * from departamentos")
  
  
  #2. Seleccionar elementos de la columna fecha de la tabla fechas
  qfechas_fecha<-dbGetQuery(conexion,"SELECT fecha FROM partosadolescentes.fechas")
  
  
  #3.	Consultar la division o descartar datos de la tabla profesionales que tengan datos en el Departamento de Amazonas(id=1)
  qdivision_profesionales_dep<-dbGetQuery(conexion,"SELECT * from partosadolescentes.porcentaje_profesionales pp
                                                    Where NOT EXISTS(
                                                    select * 
                                                    from partosadolescentes.departamentos as d
                                                    where d.nombre = 'Amazonas'
                                                    and pp.departamento_id=d.id)")
  View(qdivision_profesionales_dep)
  #4.	Consultar la division o descartar datos de la tabla profesionales que tengan datos el 2012(id=1)
  qdivision_profesionales_fecha<-dbGetQuery(conexion,"SELECT * from partosadolescentes.porcentaje_profesionales pp
                                                    Where NOT EXISTS(
                                                    select * 
                                                    from partosadolescentes.fechas as f
                                                    where f.fecha = '2012'
                                                    and pp.fecha_id=f.id)")
  
  
  #5.	Consultar el filtrado de datos en tabla procentaje_profesionales por fecha 2012   ............
  qfiltrado_fecha<-dbGetQuery(conexion,"select departamento_id, porcentaje, fecha_id
                                from partosadolescentes.porcentaje_profesionales as pp 
                                where pp.fecha_id='1'")
  
  
  #6 Consultar el filtrado de datos en tabla porcentaje_profesionales donde los porcentaje sean nulos
  qfiltrado_porcentaje<-dbGetQuery(conexion,"select departamento_id, porcentaje 
                                 from partosadolescentes.porcentaje_profesionales as pp 
                                 where pp.porcentaje is null")
  
  
  
  #7 consultar el Join de datos en la tabla porcentaje para saber el departamento donde se encuentra ese porcentaje
  qjoin_porcentaje_dep<-dbGetQuery(conexion,"select pp.id as id_porcentaje_profesional,
                                       pp.porcentaje,
                                       pd.id as id_departamento,
                                       pd.nombre as departamento 
                                       from partosadolescentes.porcentaje_profesionales as pp 
                                       join partosadolescentes.departamentos pd
                                       on pp.departamento_id = pd.id")
  
  
  #8 consultar el Join de datos en la tabla porcentaje para saber la fecha(año) cuando se realizó ese porcentjae
  qjoin_porcentaje_fec<-dbGetQuery(conexion,"select pp.id as id_porcentaje_profesional,
                                       pp.porcentaje,
                                       pf.id as id_fecha,
                                       pf.fecha as año 
                                       from partosadolescentes.porcentaje_profesionales as pp 
                                       join partosadolescentes.fechas pf
                                       on pp.fecha_id = pf.id")
  
  
  
  #9 Seleccionamos toda la tabla de porcentaje_profesionales
  df_tabla_profesionales<-dbGetQuery(conexion,"select * from partosadolescentes.porcentaje_profesionales")
  
  
  #10 Seleccionamos toda la tabla de porcentaje_emb_juv
  df_tabla_porcentaje_adolescentes_emb<-dbGetQuery(conexion,"select * from partosadolescentes.porcentaje_adolescentes_emb")
  
  
  #11 consultar el Join de datos en la tabla porcentaje para saber el departamento donde se realizó ese porcentaje
  df_prof_ados_dep<-dbGetQuery(conexion,"select 
                                       pd.nombre as Departamento,
                                       pp.id as id_porcentaje_profesional,
                                       pp.porcentaje as porcentaje_profesional,
                                       pa.id as id_porcentaje_adolescente,
                                       pa.porcentaje as porcentaje_adolescente
                                       from partosadolescentes.porcentaje_profesionales as pp 
                                       join partosadolescentes.porcentaje_adolescentes_emb as pa
                                       join partosadolescentes.departamentos pd
                                       on pp.id = pa.id and pp.departamento_id=pd.id")
  
  
  #12 consultar el Join de datos en la tabla porcentaje para saber el departamento por cada fecha(año) cuando se realizó ese porcentjae
  df_prof_fec_dep<-dbGetQuery(conexion,"select 
                                       pd.nombre as Departamento,
                                       pf.fecha as Año,
                                       pp.porcentaje as porcentaje_profesional
                                       from partosadolescentes.porcentaje_profesionales as pp 
                                       join partosadolescentes.fechas pf
                                       join partosadolescentes.departamentos pd
                                       on pp.fecha_id = pf.id and pp.departamento_id=pd.id")
  
  #13.	Consultar el filtrado de datos en tabla procentaje_profesionales por fecha 2012,amazonas y ayacucho   ............
  df_prof_depar_nom_12<-dbGetQuery(conexion,"select pd.nombre as Departamento, porcentaje, fecha_id
                                from partosadolescentes.porcentaje_profesionales as pp
                                join partosadolescentes.departamentos as pd
                                on pd.id=pp.departamento_id 
                                where pp.fecha_id='1' and pd.id < 5")
  
  
  #14 consultar el Join de datos en la tabla porcentaje para saber el departamento por cada fecha(año) cuando se realizó ese porcentjae
  df_prof_fec_dep_4p<-dbGetQuery(conexion,"select 
                                       pd.nombre as Departamento,
                                       pf.fecha as Año,
                                       pp.porcentaje as porcentaje_profesional
                                       from partosadolescentes.porcentaje_profesionales as pp 
                                       join partosadolescentes.fechas pf
                                       join partosadolescentes.departamentos pd
                                       on pp.fecha_id = pf.id and pp.departamento_id=pd.id
                                       where pd.id < 5")
  
  
  #15 Consultar el filtrado de datos en tabla porcentaje_profesionales donde los porcentaje sean nulos
  qfiltrado_porcentaje<-dbGetQuery(conexion,"select departamento_id, porcentaje 
                                 from partosadolescentes.porcentaje_profesionales as pp 
                                 where pp.porcentaje is null")
  #encontrar cantidad de nulos por años
  #16.Consultar el filtrado de datos en tabla procentaje_adloscentes por respectiva fecha y departamento
  df_emb_fecha_m4_dep_m3<-dbGetQuery(conexion,"select pd.nombre as Departamento, porcentaje, pf.fecha
                                       from partosadolescentes.porcentaje_adolescentes_emb as pa
                                       join partosadolescentes.departamentos as pd
                                       on pd.id=pa.departamento_id 
                                       Join partosadolescentes.fechas as pf
                                       on pf.id=pa.fecha_id
                                       where pa.fecha_id<'4' and pd.id < 3 ")
  
  #17.
  qjoin_porcentaje_emb<-dbGetQuery(conexion,"select pp.id as id_porcentaje_embarazadas,
                                       pp.porcentaje,
                                       pd.id as id_departamento,
                                       pd.nombre as departamento,
                                       pf.fecha 
                                       from partosadolescentes.porcentaje_adolescentes_emb as pp 
                                       join partosadolescentes.departamentos pd
                                       on pp.departamento_id = pd.id
                                       join partosadolescentes.fechas pf
                                       on pp.fecha_id = pf.id
                                       where pp.departamento_id=27")
  
  #18.
  paraKNN<-dbGetQuery(conexion,"select pp.id as id_porcentaje_embarazadas,
                                           pp.porcentaje as porcentaje_profes,
                                           pf.fecha as año,
                                           pa.porcentaje as porcentaje_adoles
                                           from partosadolescentes.porcentaje_profesionales as pp 
                                           join partosadolescentes.porcentaje_adolescentes_emb pa
                                           on pp.id= pa.id
                                           join partosadolescentes.fechas pf
                                           on pp.fecha_id = pf.id")
  
  
  qpuebra2012<-dbGetQuery(conexion,"select
pp.porcentaje as id_porcentaje_embarazadas,
pf.fecha as año
from partosadolescentes.porcentaje_profesionales as pp
join partosadolescentes.fechas pf
on pp.fecha_id = pf.id
where pf.id='1'")
  
  
  
  
  qpuebra2013<-dbGetQuery(conexion,"select
pp.porcentaje as id_porcentaje_embarazadas,
pf.fecha as año
from partosadolescentes.porcentaje_profesionales as pp
join partosadolescentes.fechas pf
on pp.fecha_id = pf.id
where pf.id='2'")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$tablaS2 <- renderTable({
    tablaS1 <- input$file1
    if (is.null(tablaS1))
    {return(NULL)}
    box<- input$select
    if(box == "1")    {
      read_excel(tablaS1$datapath)
    } else{  if(box == "2") {
      read_excel(tablaS1$datapath)
    } else { if(box == "3") {
      read_excel(tablaS1$datapath)
    } 
    } }
  })
  
  output$consulta1 <- renderText({'
  
####################### 1- Recolección de Datos #######################

file.choose()

ruta_excel<-"D:\\Documentos\\Cienciasdelacomputación\\2020-02\\Administración de datos\\Projects\\TrabajoFinalAmin\\Partos atendidos por especialistas.xlsx"

datos<- read_excel(ruta_excel, sheet=serie-partos CU-2.8, range = B18:O45)

View(datos)
names(datos)[2]="Año 2000"
names(datos)[3]="Año 2004/2006"
names(datos)[4]="Año 2007/2008"
names(datos)[5]="Año 2009"
names(datos)[6]="Año 2010"
names(datos)[7]="Año 2011"
names(datos)[8]="Año 2012"
names(datos)[9]="Año 2013"
names(datos)[10]="Año 2014"
names(datos)[11]="Año 2015"
names(datos)[12]="Año 2016"
names(datos)[13]="Año 2017"
names(datos)[14]="Año 2018"

######################## 2- Preparación de los datos ##########################
#MUESTREO-Se tomará como muestra los datos de cada region desde el año 2014 hasta el 20018
str(datos)

datosMuestra<-datos[c(1:nrow(datos)),c(1,8:14)]
View(datosMuestra)
#NORMALIZACION
names(datosMuestra)[2]="2012"
names(datosMuestra)[3]="2013"
names(datosMuestra)[4]="2014"
names(datosMuestra)[5]="2015"
names(datosMuestra)[6]="2016"
names(datosMuestra)[7]="2017"
names(datosMuestra)[8]="2018"

#IMPUTACION Y ELIMINACION DE VALORES ANOMALOS
for(i in 1:nrow(datosMuestra)){
  for(j in 1:ncol(datosMuestra)){
    datosMuestra[i,j][datosMuestra[i,j]=="-"]<-NA
  }
}
  
   ' })
  

  
  output$consulta1.2<- renderText({'
  
####################### 1- Recolección de Datos #######################


file.choose()

ruta_excel2<-"D:\\Documentos\\Cienciasdelacomputación\\2020-02\\Administración de datos\\Projects\\TrabajoFinalAmin\\Adolescentes embarazadas.xlsx"

datos2<- read_excel(ruta_excel2, sheet=mater adoles 2.10, range = B12:P39)

View(datos2)
names(datos2)[2]="Año 2000"
names(datos2)[3]="Año 2004/2006"
names(datos2)[4]="Año 2007/2008"
names(datos2)[5]="Año 2009"
names(datos2)[6]="Año 2010"
names(datos2)[7]="Año 2011"
names(datos2)[8]="Año 2012"
names(datos2)[9]="Año 2013"
names(datos2)[10]="Año 2014"
names(datos2)[11]="Año 2015"
names(datos2)[12]="Año 2016"
names(datos2)[13]="Año 2017"
names(datos2)[14]="Año 2018"

######################### 2- Preparación de los datos ########################
#MUESTREO-Se tomará como muestra los datos de cada region desde el año 2014 hasta el 20018

str(datos2)

datosMuestra2<-datos2[c(1:nrow(datos2)),c(1,8:14)]
View(datosMuestra2)
#NORMALIZACION
names(datosMuestra2)[2]="2012"
names(datosMuestra2)[3]="2013"
names(datosMuestra2)[4]="2014"
names(datosMuestra2)[5]="2015"
names(datosMuestra2)[6]="2016"
names(datosMuestra2)[7]="2017"
names(datosMuestra2)[8]="2018"
#IMPUTACION Y ELIMINACION DE VALORES ANOMALOS
for(i in 1:nrow(datosMuestra2)){
  for(j in 1:ncol(datosMuestra2)){
    datosMuestra2[i,j][datosMuestra2[i,j]=="-"]<-NA
  }
}
str(datosMuestra2)
datosMuestra2$"2012"<-as.numeric(datosMuestra2$"2012")
datosMuestra2$"2013"<-as.numeric(datosMuestra2$"2013")
datosMuestra2$"2014"<-as.numeric(datosMuestra2$"2014")
  
   ' })
  output$ConexionBaseD <- renderText({
    
    '
    
driver=MySQL()
host="35.172.128.83"
port=3306
user="admininfo"
password="admininfo"
dbname="partosadolescentes"

if(dbCanConnect(drv=driver,port=port,user=user,host=host,password=password,dbname=dbname))#Para ver si se puede conectar con la base de datos
{
  conexion<-dbConnect(drv=driver,port=port,user=user,host=host,
                      password=password,
                      dbname=dbname) 
}
View(conexion)
dbIsValid(conexion)
View(dbListTables(conexion))
    
       '
  })
  
  
  output$CreacionTablas <- renderText({
    
    '
    
#MODELADO E IMPLEMENTACION
####################Tabla de Departamentos####################
#Id para todos
id<-c(1:nrow(datosMuestra))

#Dataframe para la tabla departamentos
nombre<-list()
nombre<-c(1)
nombre<-c(nombre,datosMuestra$Departamento)
nombre<-gsub(c("á"), "a", nombre)
nombre<-gsub(c("Á"), "A", nombre)
nombre<-gsub(c("é"), "e", nombre)
nombre<-gsub(c("É"), "E", nombre)
nombre<-gsub(c("í"), "i", nombre)
nombre<-gsub(c("Í"), "I", nombre)
nombre<-gsub(c("ó"), "o", nombre)
nombre<-gsub(c("Ó"), "O", nombre)
nombre<-gsub(c("ú"), "u", nombre)
nombre<-gsub(c("Ú"), "U", nombre)
nombre<-nombre[-c(1)]

dfdepartamento<-data.frame(id,nombre)
View(dfdepartamento)

#Creando la tabla departamento con datos
query<-sqlCreateTable(conexion,"departamentos",
                      dfdepartamento)
query
#Integramos la tabla departamento a la base de datos
dbExecute(conexion, statement=query)

####################Tabla de Fechas####################

#Dataframe para la tabla fechas

id<-c(1:7)
fecha<-c("2012","2013","2014","2015","2016","2017","2018")
dffecha<-data.frame(id,fecha)

#Creando la tabla fecha con datos
query<-sqlCreateTable(conexion,"fechas",
                      dffecha)
query
#Integramos la tabla fecha a la base de datos
dbExecute(conexion, statement=query)

####################Tabla de Profesionales####################

#Dataframe para la tabla Profesionales
id<-c(1:(nrow(datosMuestra)*((ncol(datosMuestra)-1))))
id

#Creando listas vacias necesarias

departamento_id<-list()
fecha_id<-list()
porcentaje<-list()
#creando elemento guia en la lista
departamento_id<-c(1)
#llenando datos en la lista departamento_id
for (i in 1:7) {
  departamento_id<-c(departamento_id,dfdepartamento$id)
}
#Eliminar el elemnto guia, es el primero elemento
departamento_id<-departamento_id[-c(1)]

#Lenando datos en la lista porcentaje
porcentaje<-c(datosMuestra$"2012",datosMuestra$"2013",datosMuestra$"2014",
              datosMuestra$"2015",datosMuestra$"2016",datosMuestra$"2017",
              datosMuestra$"2018")
#Los datos obtenidos son cadena de caracteres hay que convertirlos a numericos
porcentaje<-as.numeric(porcentaje)

#Creando elemento guia en la lista fecha
fecha_id<-c(1)

#Llenando datos en la lista fecha_id

for (i in 1:7) {
  for (j in 1:nrow(datosMuestra)) {
    fecha_id<-c(fecha_id,i)
  }
}

#Eliminar el elemnto guia, es el primero elemento
fecha_id<-fecha_id[-c(1)]
#Creando la el data frame profesionales
dfprofesionales<-data.frame(id,departamento_id,porcentaje,fecha_id)
View(dfprofesionales)

#Creando la tabla años con datos
query<-sqlCreateTable(conexion,"porcentaje_profesionales",
                      fields=c(id="bigint",departamento_id="bigint",porcentaje="double",fecha_id="bigint"))
query
#Integramos la tabla años a la base de datos
dbExecute(conexion, statement=query)



dbWriteTable(conexion,name="porcentaje_profesionales", value=dfprofesionales, overwrite=TRUE)
dbWriteTable(conexion,name="departamentos", value=dfdepartamento, overwrite=TRUE)
dbWriteTable(conexion,name="fechas", value=dffecha, overwrite=TRUE)

####################Tabla de Adolescentes####################

#Dataframe para la tabla adolescentes
id<-c(1:(nrow(datosMuestra2)*((ncol(datosMuestra2)-1))))
id

#Creando listas vacias necesarias

departamento_id<-list()
fecha_id<-list()
porcentaje<-list()
#creando elemento guia en la lista
departamento_id<-c(1)
#llenando datos en la lista departamento_id
for (i in 1:7) {
  departamento_id<-c(departamento_id,dfdepartamento$id)
}
#Eliminar el elemnto guia, es el primero elemento
departamento_id<-departamento_id[-c(1)]

#Llenando datos en la lista porcentaje
porcentaje<-c(datosMuestra2$"2012",datosMuestra2$"2013",datosMuestra2$"2014",
              datosMuestra2$"2015",datosMuestra2$"2016",datosMuestra2$"2017",
              datosMuestra2$"2018")
#Los datos obtenidos son cadena de caracteres hay que convertirlos a numericos
porcentaje<-as.numeric(porcentaje)

#Creando elemento guia en la lista fecha
fecha_id<-c(1)

#Llenando datos en la lista fecha_id

for (i in 1:7) {
  for (j in 1:nrow(datosMuestra)) {
    fecha_id<-c(fecha_id,i)
  }
}

#Eliminar el elemnto guia, es el primero elemento
fecha_id<-fecha_id[-c(1)]
#Creando la el data frame profesionales
dfadolescentes<-data.frame(id,departamento_id,porcentaje,fecha_id)
View(dfadolescentes)

#Creando la tabla años con datos
query<-sqlCreateTable(conexion,"porcentaje_adolescentes_emb",
                      fields=c(id="bigint",departamento_id="bigint",porcentaje="double",fecha_id="bigint"))
query
#Integramos la tabla años a la base de datos
dbExecute(conexion, statement=query)



dbWriteTable(conexion,name="porcentaje_adolescentes_emb", value=dfadolescentes, overwrite=TRUE)
#dbWriteTable(conexion,name="departamentos", value=dfdepartamento, overwrite=TRUE)
#dbWriteTable(conexion,name="fechas", value=dffecha, overwrite=TRUE)
    
       '
  })
  
  
  output$tablaS5 <- renderTable({
    Pro<- read.csv(file = 'datasetFinalPre.csv')
    if (is.null(Pro)) 
      return(NULL)
    else head(Pro)
  })
  
  output$tablaS5.2 <- renderTable({
    Pro2<- read.csv(file = 'datasetFinalPre2.csv')
    if (is.null(Pro2)) 
      return(NULL)
    else head(Pro2)
  })
  
  
  
  ConsultasSQL <- 
    c("1. Seleccionar elementos de tabla departamentos qdepartamentos<-dbGetQuery(conexion,select * from departamentos)" ,
      "2. Seleccionar elementos de la columna fecha de la tabla fechas qfechas_fecha<-dbGetQuery(conexion,SELECT fecha FROM partosadolescentes.fechas)",
      "3.	Consultar la division o descartar datos de la tabla profesionales que tengan datos en el Departamento de Amazonas(id=1) qdivision_profesionales_dep<-dbGetQuery(conexion,SELECT * from partosadolescentes.porcentaje_profesionales pp Where NOT EXISTS( select * from partosadolescentes.departamentos as d where d.nombre = Amazonas and pp.departamento_id=d.id))",
      "4.	Consultar la division o descartar datos de la tabla profesionales que tengan datos el 2012(id=1) qdivision_profesionales_fecha<-dbGetQuery(conexion, SELECT * from partosadolescentes.porcentaje_profesionales pp Where NOT EXISTS( select *  from partosadolescentes.fechas as f where f.fecha = 2012 and pp.fecha_id=f.id))" ,
      "5.	Consultar el filtrado de datos en tabla procentaje_profesionales por fecha 2012 qfiltrado_fecha<-dbGetQuery(conexion,select departamento_id, porcentaje, fecha_id from partosadolescentes.porcentaje_profesionales as pp where pp.fecha_id=1)",
      "6. Consultar el filtrado de datos en tabla porcentaje_profesionales donde los porcentaje sean nulos qfiltrado_porcentaje<-dbGetQuery(conexion,select departamento_id, porcentaje  from partosadolescentes.porcentaje_profesionales as pp  where pp.porcentaje is null)",
      "7. consultar el Join de datos en la tabla porcentaje para saber el departamento donde se encuentra ese porcentaje qjoin_porcentaje_dep<-dbGetQuery(conexion,select pp.id as id_porcentaje_profesional, pp.porcentaje, pd.id as id_departamento, pd.nombre as departamento  from partosadolescentes.porcentaje_profesionales as pp  join partosadolescentes.departamentos pd on pp.departamento_id = pd.id)" ,
      "8. consultar el Join de datos en la tabla porcentaje para saber la fecha(año) cuando se realizó ese porcentjae qjoin_porcentaje_fec<-dbGetQuery(conexion,select pp.id as id_porcentaje_profesional, pp.porcentaje, pf.id as id_fecha, pf.fecha as año from partosadolescentes.porcentaje_profesionales as pp join partosadolescentes.fechas pf on pp.fecha_id = pf.id)",
      "9. Seleccionamos toda la tabla de porcentaje_profesionales df_tabla_profesionales<-dbGetQuery(conexion, select * from partosadolescentes.porcentaje_profesionales)",
      "10. Seleccionamos toda la tabla de porcentaje_emb_juv df_tabla_porcentaje_adolescentes_emb<-dbGetQuery(conexion, select * from partosadolescentes.porcentaje_adolescentes_emb) View(df_tabla_porcentaje_adolescentes_emb)")
  
  
  output$consulta3 <- renderText({
    box<- input$selectsq
    
    if (is.null(box))
    {return(NULL)}
    
    if(box == "1")    {
      ConsultasSQL[1]
    } else{  if(box == "2") {
      ConsultasSQL[2]
    } else { if(box == "3") {
      ConsultasSQL[3]
    }else { if(box == "4") {
      ConsultasSQL[4]
    }else { if(box == "5") {
      ConsultasSQL[5]
    }else { if(box == "6") {
      ConsultasSQL[6]
    }else { if(box == "7") {
      ConsultasSQL[7]
    }else { if(box == "8") {
      ConsultasSQL[8]
    }else { if(box == "9") {
      ConsultasSQL[9]
    }else { if(box == "10") {
      ConsultasSQL[10]
    }
      
    }}}}}}}}}
    
  })
  
  output$tablaS4 <- renderDataTable({
    box<- input$selectsq
    if (is.null(box))
    {return(NULL)}
    
    if(box == "1")    {
      sql1 <- dbGetQuery(conexion,"select * from departamentos")
      return (sql1)
    } else{  if(box == "2") {
      sql2 <- dbGetQuery(conexion,"SELECT fecha FROM partosadolescentes.fechas")
      return (sql2)
    } else { if(box == "3") {
      sql3 <- dbGetQuery(conexion,"SELECT * from partosadolescentes.porcentaje_profesionales pp
                                                    Where NOT EXISTS(
                                                    select * 
                                                    from partosadolescentes.departamentos as d
                                                    where d.nombre = 'Amazonas'
                                                    and pp.departamento_id=d.id)")
      
      return (sql3)
    } else { if(box == "4") {
      sql4 <- dbGetQuery(conexion,"SELECT * from partosadolescentes.porcentaje_profesionales pp
                                                    Where NOT EXISTS(
                                                    select * 
                                                    from partosadolescentes.fechas as f
                                                    where f.fecha = '2012'
                                                    and pp.fecha_id=f.id)")
      
      return (sql4)
    } else { if(box == "5") {
      sql5 <- dbGetQuery(conexion,"select departamento_id, porcentaje, fecha_id
                                from partosadolescentes.porcentaje_profesionales as pp 
                                where pp.fecha_id='1'")
      
      return (sql5)
    } else { if(box == "6") {
      sql6 <- dbGetQuery(conexion,"select departamento_id, porcentaje 
                                 from partosadolescentes.porcentaje_profesionales as pp 
                                 where pp.porcentaje is null")
      
      return (sql6)
    } else { if(box == "7") {
      sql7 <- dbGetQuery(conexion,"select pp.id as id_porcentaje_profesional,
                                       pp.porcentaje,
                                       pd.id as id_departamento,
                                       pd.nombre as departamento 
                                       from partosadolescentes.porcentaje_profesionales as pp 
                                       join partosadolescentes.departamentos pd
                                       on pp.departamento_id = pd.id")
      
      return (sql7)
    } else { if(box == "8") {
      sql8 <- dbGetQuery(conexion,"select pp.id as id_porcentaje_profesional,
                                       pp.porcentaje,
                                       pf.id as id_fecha,
                                       pf.fecha as año 
                                       from partosadolescentes.porcentaje_profesionales as pp 
                                       join partosadolescentes.fechas pf
                                       on pp.fecha_id = pf.id")
      
      return (sql8)
    } else { if(box == "9") {
      sql9 <- df_tabla_profesionales<-dbGetQuery(conexion,"select * from partosadolescentes.porcentaje_profesionales")
      
      return (sql9)
    } else { if(box == "10") {
      sql10 <- df_tabla_porcentaje_adolescentes_emb<-dbGetQuery(conexion,"select * from partosadolescentes.porcentaje_adolescentes_emb")
      
      return (sql10)
    } 
      
    }}}}}}}}}
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  ConsultaGraficas <- 
    c(
      "1.Diagrama de dispersion adolescentes vs profecionales plot(df_prof_ados_dep)",
      "2.Diagrama de dispersion de los porcentajes de adolescentes vs porcentaje de profecionales plot(df_prof_ados_dep$porcentaje_adolescente,df_prof_ados_dep$porcentaje_profesional) ",
      "3.Grafica el de antes pero en diferentes viewers\ng3 <- g2+facet_wrap(vars(DEPARTAMENTO))",
      "4.Diagrama que interpreta la cantidad de profesionales de los años 2011-2018  ggplot(data=qjoin_porcentaje_dep, aes(x=qjoin_porcentaje_dep$departamento, y=qjoin_porcentaje_dep$porcentaje)) +  geom_bar(stat=identity, position=stack)+coord_flip()+labs(x= Departamentos, y = Profesionales)",
      "5.Diagra que interpreta el porcentaje de profesionales de todos los años y de todos departamentos ggplot(df_prof_fec_dep, aes(df_prof_fec_dep$Departamento, df_prof_fec_dep$porcentaje_profesional, fill = df_prof_fec_dep$Año)) + geom_bar(stat = identity, position = dodge) + coord_flip() + labs(x= Departamentos, y = Porcentaje Profesionales, fill=Año)",
      "6.Diagrama de barras que interpreta el porcentaje de adolescente embarazadas y/ madres primerizas en los años 2012-2014 y en los departamentos de amazonas y Ancash ggplot(df_emb_fecha_m4_dep_m3, aes(fecha, df_emb_fecha_m4_dep_m3$porcentaje, fill = df_emb_fecha_m4_dep_m3$Departamento)) + geom_bar(stat = identity, position = dodge) + coord_flip() + labs(x= Fecha, y = Porcentaje embarazo adolescente, fill=Departamento)",
      "7.Diagrama de barras que interpreta el porcentaje de porcentaje de embarazo adolescente y/ madres primerizas en los años 2012-2018 ggplot(qjoin_porcentaje_emb, aes(qjoin_porcentaje_emb$departamento, qjoin_porcentaje_emb$porcentaje, fill = qjoin_porcentaje_emb$fecha)) + geom_bar(stat = identity, position = dodge) + coord_flip() + labs(x= Departamentos, y = Porcentaje embarazo adolescente, fill=Año)",
      "8.Diagrama de barras que interpreta el porcentaje de porcentaje de embarazo adolescente y/ madres primerizas en los años 2012-2018 ggplot(data=qjoin_porcentaje_emb, aes(x=qjoin_porcentaje_emb$fecha, y=qjoin_porcentaje_emb$porcentaje)) + geom_bar(stat=identity, position=stack)+labs(x= Años, y = Porcentaje embarazo adolescente)",
      "9.Diagrama de cajas boxplot(formula = df_prof_fec_dep$porcentaje_profesional ~ df_prof_fec_dep$Año, data =  df_prof_fec_dep,  xlab = Año, ylab = Porcentaje de profesionales, col = c(orange3, yellow3, green3, grey,red3,blue3,brown3))",
      "10.boxplot(formula = df_prof_fec_dep_4p$porcentaje_profesional ~ df_prof_fec_dep_4p$Departamento, data =  df_prof_fec_dep_4p,  xlab = Departamento, ylab = Porcentaje de profesionales)"
      
    )
  
  
  output$consulta4 <- renderText({
    
    if (input$selectgg == "1"){
      
      ConsultaGraficas[1]
    }
    
    else {if (input$selectgg == "2") {
      ConsultaGraficas[2]
    }
      
      else { if (input$selectgg == "3"){
        ConsultaGraficas[3]
      }
        
        else{ if (input$selectgg == "4"){
          ConsultaGraficas[4]
        }
          
          else{ if (input$selectgg == "5"){
            ConsultaGraficas[5]
          }
            
            else{ if (input$selectgg == "6"){
              ConsultaGraficas[6]
            }
              else{ if (input$selectgg == "7"){
                ConsultaGraficas[7]
              }
                
                else{ if (input$selectgg == "8"){
                  ConsultaGraficas[8]
                }
                  else {if (input$selectgg == "9"){
                    ConsultaGraficas[9]
                  }
                    else{ if (input$selectgg == "10"){
                      ConsultaGraficas[10]
                    }
                      
                      
                    }}}}}}}}} 
    
    
  })
  
  output$plot1 <- renderPlot({
    box<- input$selectgg
    if (is.null(box))
    {return(NULL)}
    
    if(box == "1")    {
      g1 <- plot(df_prof_ados_dep)
      return (g1)
    } else{  if(box == "2") {
      g2 <- plot(df_prof_ados_dep$porcentaje_adolescente,df_prof_ados_dep$porcentaje_profesional)
      return (g2)
    } else { if(box == "3") {
      g3 <- ggplot(data=qjoin_porcentaje_dep, aes(x=qjoin_porcentaje_dep$departamento, y=qjoin_porcentaje_dep$porcentaje)) + 
        geom_bar(stat="identity", position="stack")+coord_flip()+labs(x= "Departamentos", y = "Profesionales")
      return (g3)
    } else { if(box == "4")
    { g4 <- ggplot(df_prof_fec_dep, aes(df_prof_fec_dep$Departamento, df_prof_fec_dep$porcentaje_profesional, fill = df_prof_fec_dep$Año)) +
      geom_bar(stat = "identity", position = "dodge") + coord_flip() + labs(x= "Departamentos", y = "Porcentaje Profesionales", fill="Año")
    return (g4)
    } else { if(box=="5")
    { g5 <- ggplot(df_emb_fecha_m4_dep_m3, aes(fecha, df_emb_fecha_m4_dep_m3$porcentaje, fill = df_emb_fecha_m4_dep_m3$Departamento)) +
      geom_bar(stat = "identity", position = "dodge") + coord_flip() + labs(x= "Fecha", y = "Porcentaje embarazo adolescente", fill="Departamento")
    return (g5)
    }
      else{ if(box == "6"){
        g6 <- ggplot(qjoin_porcentaje_emb, aes(qjoin_porcentaje_emb$departamento, qjoin_porcentaje_emb$porcentaje, fill = qjoin_porcentaje_emb$fecha)) +
          geom_bar(stat = "identity", position = "dodge") + coord_flip() + labs(x= "Departamentos", y = "Porcentaje embarazo adolescente", fill="Año")
        
        return (g6)
      }
        else{ if(box == "7"){
          g7 <- ggplot(data=qjoin_porcentaje_emb, aes(x=qjoin_porcentaje_emb$fecha, y=qjoin_porcentaje_emb$porcentaje)) + 
            geom_bar(stat="identity", position="stack")+labs(x= "Años", y = "Porcentaje embarazo adolescente")
          return (g7)
        }
          else{ if(box == "8"){
            g8 <- boxplot(formula = df_prof_fec_dep$porcentaje_profesional ~ df_prof_fec_dep$Año, data =  df_prof_fec_dep,  xlab = "Año", ylab = "Porcentaje de profesionales", 
                          col = c("orange3", "yellow3", "green3", "grey","red3","blue3","brown3"))
            return (g8)
          }
            else{ if(box == "9"){
              g9 <- boxplot(formula = df_prof_fec_dep_4p$porcentaje_profesional ~ df_prof_fec_dep_4p$Departamento, data =  df_prof_fec_dep_4p,  xlab = "Departamento", ylab = "Porcentaje de profesionales")
              return (g9)
            }
              else{ if(box == "10"){
                
                g10 <- boxplot(formula = qjoin_porcentaje_emb$porcentaje ~ qjoin_porcentaje_emb$departamento, data =  qjoin_porcentaje_emb,  xlab = "Departamento de Ucayali", ylab = "Porcentaje de profesionales")

                return (g10)
              }
                
              }}}}}}}}} 
  })
  
  
  
  
  
  
  
  
  ################################Regresion lineal############################
  
  qpuebra2012<-dbGetQuery(conexion,"select
pp.porcentaje as id_porcentaje_embarazadas,
pf.fecha as año
from partosadolescentes.porcentaje_profesionales as pp
join partosadolescentes.fechas pf
on pp.fecha_id = pf.id
where pf.id='1'")
  View(qpuebra2012)
  
  
  
  qpuebra2013<-dbGetQuery(conexion,"select
pp.porcentaje as id_porcentaje_embarazadas,
pf.fecha as año
from partosadolescentes.porcentaje_profesionales as pp
join partosadolescentes.fechas pf
on pp.fecha_id = pf.id
where pf.id='2'")
  View(qpuebra2013)
  
  #####
  regresion<-function(x,y, nx=NA, ny=NA){
    resultado<-list()
    prediccion<-NA
    n<-NROW(x)
    promX<-mean(x)
    promY<-mean(y)
    (dx<-x-promX)
    (dy<-y-promY)
    (xy<-dx*dy)
    #cálculo de la covarianza
    cov<-sum(xy)/(n-1)#cov(x,y)
    dx2<-dx^2
    dy2<-dy^2
    #cálculo de las desviaciones estandar
    sdX<-sqrt(sum(dx2)/(n-1))#  sd(x)
    sdY<-sqrt(sum(dy2)/(n-1))# sd(y)
    # calculo del coeficiente correlacional
    (r<-cov/(sdX*sdY))
    #creando la prediccion regresional x~y
    if(is.na(ny))
      prediccion<-promY+cov*(nx-promX)/(sdX^2)
    #creando la prediccion regresional y~x
    if(is.na(nx))
      prediccion<-promX+cov*(ny-promY)/(sdY^2)
    
    resultado[[1]]<-data.frame(x,y,dx,dy,xy,dx2,dy2)
    resultado[[2]]<-cov
    resultado[[3]]<-r
    resultado[[4]]<-prediccion
    return (resultado)
  }
  ######
  
  qpuebra2012$id_porcentaje_embarazadas[is.na(qpuebra2012$id_porcentaje_embarazadas)] <- 0
  qpuebra2013$id_porcentaje_embarazadas[is.na(qpuebra2013$id_porcentaje_embarazadas)] <- 0
  
  df2012_2013<-data.frame(qpuebra2012,qpuebra2013)
  regresion(qpuebra2012$id_porcentaje_embarazadas,qpuebra2013$id_porcentaje_embarazadas,nx=12.2)
  
  ggplot(df2012_2013, aes(x=df2012_2013$id_porcentaje_embarazadas, y=df2012_2013$id_porcentaje_embarazadas.1)) + geom_point() + ggtitle("Gráfica de Regresion") + xlab("Numero de casos denunciados por mujeres") + ylab("Numero de casos denunciados por violencia fisica") + geom_smooth(method=lm)
  
  
  
  
  
  
  
  
  
  output$CodRL <- renderText({
    '
    
  ################################Regresion lineal############################

qpuebra2012<-dbGetQuery(conexion,"select
pp.porcentaje as id_porcentaje_embarazadas,
pf.fecha as año
from partosadolescentes.porcentaje_profesionales as pp
join partosadolescentes.fechas pf
on pp.fecha_id = pf.id
where pf.id=1")
View(qpuebra2012)



qpuebra2013<-dbGetQuery(conexion,"select
pp.porcentaje as id_porcentaje_embarazadas,
pf.fecha as año
from partosadolescentes.porcentaje_profesionales as pp
join partosadolescentes.fechas pf
on pp.fecha_id = pf.id
where pf.id=2")
View(qpuebra2013)

#####
regresion<-function(x,y, nx=NA, ny=NA){
  resultado<-list()
  prediccion<-NA
  n<-NROW(x)
  promX<-mean(x)
  promY<-mean(y)
  (dx<-x-promX)
  (dy<-y-promY)
  (xy<-dx*dy)
  #cálculo de la covarianza
  cov<-sum(xy)/(n-1)#cov(x,y)
  dx2<-dx^2
  dy2<-dy^2
  #cálculo de las desviaciones estandar
  sdX<-sqrt(sum(dx2)/(n-1))#  sd(x)
  sdY<-sqrt(sum(dy2)/(n-1))# sd(y)
  # calculo del coeficiente correlacional
  (r<-cov/(sdX*sdY))
  #creando la prediccion regresional x~y
  if(is.na(ny))
    prediccion<-promY+cov*(nx-promX)/(sdX^2)
  #creando la prediccion regresional y~x
  if(is.na(nx))
    prediccion<-promX+cov*(ny-promY)/(sdY^2)
  
  resultado[[1]]<-data.frame(x,y,dx,dy,xy,dx2,dy2)
  resultado[[2]]<-cov
  resultado[[3]]<-r
  resultado[[4]]<-prediccion
  return (resultado)
}
######

qpuebra2012$id_porcentaje_embarazadas[is.na(qpuebra2012$id_porcentaje_embarazadas)] <- 0
qpuebra2013$id_porcentaje_embarazadas[is.na(qpuebra2013$id_porcentaje_embarazadas)] <- 0

df2012_2013<-data.frame(qpuebra2012,qpuebra2013)
regresion(qpuebra2012$id_porcentaje_embarazadas,qpuebra2013$id_porcentaje_embarazadas,nx=12.2)

ggplot(df2012_2013, aes(x=df2012_2013$id_porcentaje_embarazadas, y=df2012_2013$id_porcentaje_embarazadas.1)) + geom_point() + ggtitle("Gráfica de Regresion") + xlab("Numero de casos denunciados por mujeres") + ylab("Numero de casos denunciados por violencia fisica") + geom_smooth(method=lm)

  '
  })
  
  
  #Implementacion de Regresion lineal
  
  output$plotRL <- renderPlot({
    
    return( 
      
      ggplot(df2012_2013, aes(x=df2012_2013$id_porcentaje_embarazadas, y=df2012_2013$id_porcentaje_embarazadas.1)) + geom_point() + ggtitle("Gráfica de Regresion") + xlab("Porcentaje de adolescentes madres primerizas en el Perú") + ylab("Porcentaje profesionales en partos en el Perú") + geom_smooth(method=lm)
    )
    
  })
  
  
  output$IdPrediccionRL <- renderText({
    
    v1 <- input$xRL
    
    resultado <-  regresion(qpuebra2012$id_porcentaje_embarazadas,qpuebra2013$id_porcentaje_embarazadas,v1 )
    
    
    as.character(resultado[[4]])
    
  })
  
  ###Codigo KNN
  
  
  paraKNN$porcentaje_profes[is.na(paraKNN$porcentaje_profes)] <- 0
  paraKNN$porcentaje_adoles[is.na(paraKNN$porcentaje_adoles)] <- 0
  #
  ggplot(data = paraKNN,aes(x=paraKNN$porcentaje_profes,y=paraKNN$porcentaje_adoles,color=paraKNN$año))+
    geom_point()+xlab("X")+ylab("Y")+ggtitle("Clasificador KNN")
  
  ids=sample(1:nrow(paraKNN),0.85*nrow(paraKNN))
  paraKNNEnt<-paraKNN[ids,]
  nrow(paraKNNEnt)
  paraKNNTest<-paraKNN[-ids,]
  nrow(paraKNNTest)
  ggplot(data = paraKNNEnt ,aes(x=paraKNNEnt$porcentaje_profes,y=paraKNNEnt$porcentaje_adoles,color=paraKNNEnt$año))+
    geom_point()+xlab("X")+ylab("Y")+ggtitle("Clasificador KNN")
  
  paraKNNTemp=paraKNN
  
  
  knn<-function(paraKNNTemp,newX,newY,k, method){
    if(method==1){
      d<-(abs(newX-paraKNNTemp$porcentaje_profes)+abs(newY-paraKNNTemp$porcentaje_adoles))    
    }else{
      d<-sqrt((newX-paraKNNTemp$porcentaje_profes)^2+(newY-paraKNNTemp$porcentaje_adoles)^2)  
    }
    paraKNNTemp<-cbind(paraKNNTemp,d)
    paraKNNTemp  
    vOrden<-sort(paraKNNTemp$d)
    vecinos<-paraKNNTemp[paraKNNTemp$d %in% vOrden[1:k],3]
    return (vecinos[1:k] )
  }
  
  v<-knn(paraKNNEnt,93.2,24.9,5,2)
  
  
  
  
  
  output$tablaKNN <- renderTable({
    
    xknn <- input$xKnn
    yknn <- input$yKnn
    kknn <- input$kKnn
    
    return (knn(paraKNNEnt , xknn, yknn , kknn,2))
    
    
  })
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)