pathrel = getwd()
#pathrel = paste0(pathrel,"/Analisis_desercion_en_ingenieria")
library(ProjectTemplate)
options(ProjectTemplate.templatedir = paste0(pathrel))
create.project(paste0(pathrel, '/desercion'), template= 'R_template_proyecto_base')

getOption('ProjectTemplate.templatedir')

setwd(paste0(pathrel,"/desercion"))
getwd()

###########################################################
# arranca el proyecto
###########################################################

load.project()

show.project()



