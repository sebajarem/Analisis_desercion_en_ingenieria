# estadisticos de datos iniciales

library(knitr)
library(kableExtra)

####
# alumnos
####

# estadisticos general
# aux = estadisticos(datos = alumnos, top = 5, generar_xlsx = F)

# estadisticos
aux = describe_variable_tipo(alumnos)
obs = nrow(alumnos)

tabla2latex_describe_variables(alumnos,
                               nombre_data = "Dataset Alumnos",
                               caption_tabla = "Campos Tabla Alumnos",
                               archivo_salida = "alumnos_describe_campos.tex")

tabla2latex_describe_variables(finales,
                               nombre_data = "Dataset Cursadas",
                               caption_tabla = "Campos Tabla Cursadas",
                               archivo_salida = "cursadas_describe_campos.tex")

tabla2latex_describe_variables(finales,
                               nombre_data = "Dataset Finales",
                               caption_tabla = "Campos Tabla Finales",
                               archivo_salida = "finales_describe_campos.tex")

# analisis puntuales de campos para explicar en el informe
names(alumnos)

unique(alumnos$Estudios.Secundarios)

names(cursada)
unique(cursada$Departamento)
unique(cursada$Modalidad)
unique(cursada$Turno)
unique(cursada$Ciclo.Lectivo.de.Cursada)
unique(cursada$Tipo.de.aprobación)
unique(cursada$Cantidad.de.veces.recursada.regular)
unique(cursada$Descripción.de.recursada.regular)

names(finales)
unique(finales$Promociono)


###
# exploracion de datos

# datos alumnos
glimpse(alumnos)

unique(sort(alumnos$Localidad))

alumnos =
  alumnos %>%
  mutate(
    Codigo.Alumno = as.factor(Codigo.Alumno)
  )

aux = estadisticos_calidad(datos = alumnos,
                           variables_excluir = c("Codigo.Alumno"),
                           generar_xlsx = F)

aux = estadisticos_numericos(datos = alumnos,
                             variables_excluir = c("Codigo.Alumno"),
                             generar_xlsx = F)

alumnos.categoricos = estadisticos_categoricos(datos = alumnos,
                               variables_excluir = c("Codigo.Alumno"),
                               top = 3,
                               generar_xlsx = F)


tabla2latex(df = alumnos.categoricos,
                    archivo_salida = "alumnos_estadisticos_categoricos.tex",
                    nombre_data = "Alumnos Estadisticos Categoricos",
                    caption_tabla = "Tabla Alumnos, valores mas frecuentes")




glimpse(cursada)

cursadas.categoricos = estadisticos_categoricos( datos = cursada,
                                                 variables_excluir = c("Codigo.Alumno"),
                                                 top = 3,
                                                 generar_xlsx = F)

cursadas.numericos = estadisticos_numericos( datos = cursada,
                                             variables_excluir = c("Codigo.Alumno"),
                                             generar_xlsx = F)

tabla2latex(df = cursadas.categoricos,
            archivo_salida = "cursadas_estadisticos_categoricos.tex",
            caption_tabla = "Tabla Cursadas, valores más frecuentes",
            nombre_data = "Cursadas Estadisticos Categoricos")

tabla2latex_scale(df = cursadas.numericos[,1:10],
            archivo_salida = "cursadas_estadisticos_numericos_1.tex",
            caption_tabla = "Tabla Cursadas, valores más frecuentes",
            nombre_data = "Cursadas Estadisticos Numericos 1de2")

tabla2latex_scale(df = cursadas.numericos[,c(1,11:16)],
            archivo_salida = "cursadas_estadisticos_numericos_2.tex",
            caption_tabla = "Tabla Cursadas, valores más frecuentes",
            nombre_data = "Cursadas Estadisticos Numericos 2de2")

glimpse(finales)

finales =
  finales %>%
  mutate(
    Nota = as.numeric(Nota),
    Año = as.numeric(Año)
  )

finales.categoricos = estadisticos_categoricos( datos = finales,
                                                 variables_excluir = c("Codigo.Alumno"),
                                                 top = 3,
                                                 generar_xlsx = F)

finales.numericos = estadisticos_numericos( datos = finales,
                                             variables_excluir = c("Codigo.Alumno"),
                                             generar_xlsx = F)

tabla2latex(df = finales.categoricos,
            archivo_salida = "finales_estadisticos_categoricos.tex",
            caption_tabla = "Tabla Finales, valores más frecuentes",
            nombre_data = "Finales Estadisticos Categoricos")

tabla2latex_scale(df = finales.numericos[,1:10],
            archivo_salida = "finales_estadisticos_numericos_1.tex",
            caption_tabla = "Tabla Finales, valores más frecuentes",
            nombre_data = "Finales Estadisticos Numericos 1de2")

tabla2latex_scale(df = finales.numericos[,c(1,11:16)],
            archivo_salida = "finales_estadisticos_numericos_2.tex",
            caption_tabla = "Tabla Finales, valores más frecuentes",
            nombre_data = "Finales Estadisticos Numericos 2de2")


# para calidad de datos

alumnos.calidad = estadisticos_calidad(datos = alumnos,
                                       variables_excluir = c("Codigo.Alumno"),
                                       generar_xlsx = F)

tabla2latex_scale(df = alumnos.calidad,
            archivo_salida = "alumnos_estadisticos_calidad.tex",
            caption_tabla = "Tabla Alumnos, valores unicos y nulos",
            nombre_data = "alumnos calidad")

cursada.calidad = estadisticos_calidad(datos = cursada,
                                       variables_excluir = c("Codigo.Alumno"),
                                       generar_xlsx = F)

tabla2latex_scale(df = cursada.calidad,
            archivo_salida = "cursada_estadistico_calidad.tex",
            caption_tabla = "Tabla Cursada, valores únicos y nulos",
            nombre_data = "cursada calidad")

finales.calidad = estadisticos_calidad(datos = finales,
                                       variables_excluir = c("Codigo.Alumno"),
                                       generar_xlsx = F)

tabla2latex_scale(df = finales.calidad,
            archivo_salida = "finales_estadisticos_calidad.tex",
            caption_tabla = "Tabla Finales, valores unicos y nulos",
            nombre_data = "finales calidad")

####
# algunos graficos


