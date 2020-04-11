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

config$path_docs_version_tablas

tabla2latex_describe_variables = function(df, 
                                          directorio = config$path_docs_version_tablas,
                                          archivo_salida = "default.tex", 
                                          caption_tabla = NA,
                                          nombre_data = NA,
                                          tamanio_fuente = 10){
  
  obs = nrow(df)
  aux = describe_variable_tipo(alumnos)
  
  aux.latex = knitr::kable(aux, format = "latex", caption = caption_tabla, booktabs =T) %>%
    kableExtra::kable_styling(latex_options = c("striped", "hold_position"),
                              font_size = tamanio_fuente) %>%
    kableExtra::row_spec(0, bold = T, color = "white", background = "black", align = "c") %>%
    kableExtra::add_header_above(c(glue("{nombre_data}"), glue("Observaciones: {obs}")))
  
  capture.output(aux.latex, file = paste0(directorio, "/",archivo_salida))
  
  return(aux.latex)
  
}

tabla2latex_describe_variables(alumnos,
                               nombre_data = "Alumnos",
                               caption_tabla = "Campos Tabla Alumnos",
                               archivo_salida = "alumnos_describe_campos.tex")


aux.latex = knitr::kable(aux, format = "latex", caption = "Campos Tabla Alumnos", booktabs =T) %>%
  kableExtra::kable_styling(latex_options = c("striped", "hold_position"),
                            font_size = 10) %>%
  kableExtra::row_spec(0, bold = T, color = "white", background = "black", align = "c") %>%
  kableExtra::add_header_above(c("Dataset Alumnos", glue("Observaciones: {obs}")))
capture.output(aux.latex, file = "./docs/00_datos_01/tablas/alumnos_describe_campos.tex")



  
aux = estadisticos_categoricos(datos = alumnos, top = 5, generar_xlsx = F, variables_excluir = "Codigo.Alumno")




tabla.tex.file = aux.latex %>% capture.output()
write_file(path = "./docs/00_datos_01/tablas/aux.tex", x = tabla.tex.file)

kableExtra::save_kable(aux.latex, file = "./docs/00_datos_01/tablas/aux.tex", bs_theme = "simplex")

