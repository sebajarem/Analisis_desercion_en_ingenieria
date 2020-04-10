# unico archivo que hay que correr para la ejecucion completa o carga de projectos

library('ProjectTemplate')
library(dplyr)
library(stringr)
library(glue)

# primer paso: configurar archivos './lib/globals.R' y './config/global.dcf'

# opcion 1:
# creo version (si no exite), carga datos y corre preprocesamiento munge correspondiente (si esta seteado en TRUE)
# load.project()

# opcion2:
# solo corre archivos del src correspondiente
# run.project()

# eliminacion de versiones:
# proxima version

# la otra opcion es cargar el proyecto y modificar los parametros al mismo tiempo
# se esta forma nos evitamos modificar loo archivos

# veriones actuales: 
# "0" "1" => pruebas de funcionamiento project Template
# "00_Datos" "00" => analisis de datos y preprocesamiento de datos

print("El menu solo sirve para las versiones que respetan el siguiente formato:")
print("<numero><numero>_<nombre>_<numero><numero>")
print("ejemplo: 00_ejemplo_01")

# MENU
opcion1_nuevo_existente = c("Correr version existente", "crear nueva version")
opcion2_versiones = sort(list.dirs(path = "./data"))[-1]
opcion_TRUE_FALSE = c(TRUE, FALSE)

switch(
  menu(
  title = "Proyecto: Desercion en ingenieria",
  graphics = F,
  choices = opcion1_nuevo_existente
  ),
  # opcion de version existente
  salida = {
    opcion1 = 1
    opcion2 = menu(
      title = "Versiones existentes",
      graphics = T,
      choices = opcion2_versiones
      )
    opcion3 = menu(
      title = "cargar data ?",
      graphics = T,
      choices = opcion_TRUE_FALSE
      )
    opcion4 = menu(
      title = "preprocesar ?",
      graphics = T,
      choices = opcion_TRUE_FALSE
      )
    opcion5 = menu(
      title = "log ?",
      graphics = T,
      choices = opcion_TRUE_FALSE
    )
  },
  # opcion de crear nueva version
  salida = {
    opcion1 = 2
    nombre_nueva_version = readline(prompt = "escriba nombre de nueva version:")
  }
)

if(opcion1 == 1){
  cadena = glue("{opcion2_versiones[{opcion2}]}")
  versionData = {
    l = str_remove(string = cadena, pattern = "./data/") %>%
      str_split(string = ., pattern = "_")
    l = unlist(l) 
    l1 = str_c(l[1], l[2], sep = "_")
  }
  versionDataNro= l[3]
}
if(opcion1 == 2){
  cadena = nombre_nueva_version
  versionData = {
    l = str_split(string = cadena, pattern = "_")
    l = unlist(l) 
    l1 = str_c(l[1], l[2], sep = "_")
  }
  versionDataNro= l[3]
}
 

if(opcion1 == 1) {
  if(opcion2 == 0 || opcion3 == 0 || opcion4 == 0 || opcion5 == 0){
    print("opcion 0 ingresada. saliendo")
  }else{
    load.project(
      versionData = glue("{versionData}"),
      versionDataNro= glue("{versionDataNro}"),
      
      data_loading= glue("{opcion_TRUE_FALSE[{opcion3}]}"), 
      data_loading_header= glue("{opcion_TRUE_FALSE[{opcion3}]}"),
      data_ignore="",
      cache_loading= glue("{opcion_TRUE_FALSE[{opcion3}]}"),
      
      munging= glue("{opcion_TRUE_FALSE[{opcion4}]}"),
      
      logging= glue("{opcion_TRUE_FALSE[{opcion5}]}"),
      logging_level= "INFO",
      load_libraries= TRUE
    )
  }
}

if(opcion1 == 2) {
  load.project(
    versionData = glue("{versionData}"),
    versionDataNro= glue("{versionDataNro}")
  )
}

if(opcion1 == 0) {
  print("opcion 0 ingresada. saliendo")
}

switch(menu(
  title = "correr proyecto ?",
  choices = c("si", "no"),
  graphics = T
  ),
  source("./src/01_correr_version_seleccionada.R"))


# load.project(
#   versionData="00_Datos",
#   versionDataNro="01",
#   
#   munging= TRUE,
#   
#   data_loading= TRUE,
#   data_loading_header= TRUE,
#   data_ignore="",
#   cache_loading= TRUE,
#   
#   logging= FALSE,
#   logging_level= "INFO",
#   load_libraries= TRUE
# )

# luego podemos correr los archivos correspondientes del src
# source("./src/01_correr_version_seleccionada.R")

# si queremos ejecutar algun archivo en particular utilizamos la funcion source() o eval()
