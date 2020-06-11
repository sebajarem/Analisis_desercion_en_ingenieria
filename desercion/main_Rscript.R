# aca hacer un script parecido al "main_conMenu.R"
# pero que se le ingresen los valores por consola de linux para correr con Rscript
library(ProjectTemplate)
library(stringr)
# library(tidyverse)
# library(dplyr)
library(glue)

args = commandArgs(trailingOnly=TRUE)

cadena = sort(list.dirs(path = "./data"))[-1]
cadena = str_remove(string = cadena, pattern = "./data/")
versiones_existentes = cadena

# argumentos
if (length(args)==0) {
  message("la forma de ejecutar este script es escribiendo como argumentos el nombre de la version a ejecutar seguido de aquellas partes del proceso que se quieren ejecutar")
  message(paste("versiones existentes: "))
  print(cadena)
  message("etapas del proceso: ")
  message("    data: carga datos con headers")
  message("    munge: hace preprocesamiento")
  message("    src: corre archivos source")
  message("    log: genera logueo")
  message("ejemplo1:   Rscript main_Rscript.R 00_ejemplo_01 data cache munge src log")
  message("ejemplo2:   Rscript main_Rscript.R 00_ejemplo_01 data munge")
  stop("vuelva a ejecutar", call.=FALSE)
}




# args[1] = "00_datos_01"

# any("00_datos_01" %in% cadena)

# print(args)
# print("empieza")

if(any(as.character(args[1]) %in% cadena)){
  cadena = args[1]
  versionData = {
    l = str_split(string = cadena, pattern = "_")
    l = unlist(l) 
    l1 = str_c(l[1], l[2], sep = "_")
  }
  versionDataNro= l[3]
}else{
  message("no existe esa version")
  message("versiones existentes:")
  print(cadena)
  stop("vuelva a ejecutar", call.=FALSE)
}


data = ifelse(any("data" %in% args),TRUE,FALSE)
munge = ifelse(any("munge" %in% args),TRUE,FALSE)
src = ifelse(any("src" %in% args),TRUE,FALSE)
log = ifelse(any("log" %in% args),TRUE,FALSE)
cache = ifelse(any("cache" %in% args),TRUE,FALSE)


# genero lista para data ignore que no sea del proyecto seleciconado por la recursividad

# agrego el ignore de otros proyectos
l_ignore = versiones_existentes[as.character(args[1]) != versiones_existentes]
l_ignore = str_remove(string = l_ignore, pattern = "./data/")
l_ignore = str_c(l_ignore, "/, ", sep = "")
l_ignore = paste(l_ignore, collapse = '')

print(paste("lista ignore:",l_ignore))

load.project(
  versionData = glue("{versionData}"),
  versionDataNro= glue("{versionDataNro}"),
  
  data_loading= glue("{data}"), 
  data_loading_header= glue("{data}"),
  data_ignore= glue("{l_ignore}"),
  recursive_loading = TRUE,
  cache_loading= glue("{cache}"),
  
  munging= glue("{munge}"),
  
  logging= glue("{log}"),
  logging_level= "INFO",
  load_libraries= TRUE
)

if(src == T){
  source("./src/01_correr_version_seleccionada.R")
}


