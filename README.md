# Analisis_desercion_en_ingenieria
Trabajo de Especialización en Minería de Datos.



## docker RStudio server
docker run -d -p 8787:8787 -v $(pwd):/home/rstudio -e PASSWORD=yourpasswordhere rocker/rstudio

se puede agregar un shiny. 
ver [documentacion](https://hub.docker.com/r/rocker/rstudio)

