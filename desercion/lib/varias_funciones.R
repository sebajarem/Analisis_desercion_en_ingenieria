
describe_variable_tipo = function(df){
  
  df.describe = data_frame(
    variable = colnames(df),
    tipo = sapply(df, typeof)
  )
  
  return(df.describe)
}

tabla2latex_describe_variables = function(df, 
                                          directorio = config$path_docs_version_tablas,
                                          archivo_salida = "default.tex", 
                                          caption_tabla = NA,
                                          nombre_data = NA,
                                          tamanio_fuente = 10){
  
  obs = nrow(df)
  aux = describe_variable_tipo(df)
  
  aux.latex = knitr::kable(aux, format = "latex", caption = caption_tabla, booktabs =T,  
                           label = paste0("tabla_", str_replace_all(nombre_data, pattern = " ", replacement = "_"))) %>%
    kableExtra::kable_styling(latex_options = c("striped", "hold_position"),
                              font_size = tamanio_fuente) %>%
    kableExtra::row_spec(0, bold = T, color = "white", background = "black", align = "c") %>%
    kableExtra::add_header_above(c(glue("{nombre_data}"), glue("Observaciones: {obs}")))
  
  capture.output(aux.latex, file = paste0(directorio, "/",archivo_salida))
  
  return(aux.latex)
  
}

tabla2latex_dataset = function(df, 
                      directorio = config$path_docs_version_tablas,
                      archivo_salida = "default.tex", 
                      caption_tabla = NA,
                      nombre_data = NA,
                      tamanio_fuente = 10){
  
  obs = nrow(df)
  
  aux.latex = knitr::kable(df, format = "latex", caption = caption_tabla, booktabs =T,  
                           label = paste0("tabla_", str_replace_all(nombre_data, pattern = " ", replacement = "_"))) %>%
    kableExtra::kable_styling(latex_options = c("striped", "hold_position"),
                              font_size = tamanio_fuente) %>%
    kableExtra::row_spec(0, bold = T, color = "white", background = "black", align = "c") %>%
    kableExtra::add_header_above(c(glue("{nombre_data}"), glue("Observaciones: {obs}")))
  
  capture.output(aux.latex, file = paste0(directorio, "/",archivo_salida))
  
  return(aux.latex)
  
}


tabla2latex = function(df, 
                               directorio = config$path_docs_version_tablas,
                               archivo_salida = "default.tex", 
                               caption_tabla = NA,
                               nombre_data = NA,
                               tamanio_fuente = 10){
  
  aux.latex = knitr::kable(df, format = "latex", caption = caption_tabla, booktabs =T,  
                           label = paste0("tabla_", str_replace_all(nombre_data, pattern = " ", replacement = "_"))) %>%
    kableExtra::kable_styling(latex_options = c("striped", "hold_position"),
                              font_size = tamanio_fuente) %>%
    kableExtra::row_spec(0, bold = T, color = "white", background = "black", align = "c")
  
  capture.output(aux.latex, file = paste0(directorio, "/",archivo_salida))
  
  return(aux.latex)
  
}


tabla2latex_scale = function(df, 
                       directorio = config$path_docs_version_tablas,
                       archivo_salida = "default.tex", 
                       caption_tabla = NA,
                       nombre_data = NA,
                       tamanio_fuente = 10){
  
  aux.latex = knitr::kable(df, format = "latex", caption = caption_tabla, booktabs =T,  
                           label = paste0("tabla_", str_replace_all(nombre_data, pattern = " ", replacement = "_"))) %>%
    kableExtra::kable_styling(latex_options = c("striped", "hold_position", "scale_down"),
                              font_size = tamanio_fuente) %>%
    kableExtra::row_spec(0, bold = T, color = "white", background = "black", align = "c")
  
  capture.output(aux.latex, file = paste0(directorio, "/",archivo_salida))
  
  return(aux.latex)
  
}



tablalarga2latex = function(df, 
                       directorio = config$path_docs_version_tablas,
                       archivo_salida = "default.tex", 
                       caption_tabla = NA,
                       nombre_data = NA,
                       tamanio_fuente = 10){
  
  aux.latex = knitr::kable(df, format = "latex", longtable = T, caption = caption_tabla, booktabs =T,  
                           label = paste0("tabla_", str_replace_all(nombre_data, pattern = " ", replacement = "_"))) %>%
    kableExtra::kable_styling(latex_options = c("striped", "hold_position", "repeat_header"),
                              font_size = tamanio_fuente) %>%
    kableExtra::row_spec(0, bold = T, color = "white", background = "black", align = "c")
  
  capture.output(aux.latex, file = paste0(directorio, "/",archivo_salida))
  
  return(aux.latex)
  
}



contar_desertores <- function(cluster_membership, datos_cluster, datos_referencia){
  
  l =
    lapply(
      X = split(row.names(datos_cluster), cluster_membership),
      FUN = function(x){
        
        df_aux =
          data.frame(codigo.alumno = c(x))
        
        df_aux =
          df_aux %>%
          inner_join(datos_referencia, by = "codigo.alumno") %>%
          select(deserto) %>%
          mutate(
            deserto = as.character(if_else(deserto == "1", "S", "N"))
          ) %>%
          group_by(deserto) %>%
          dplyr::summarise(
            cantidad = dplyr::n()
          )
        
        
      }
    )
  
  
  dd =
    tibble(
      num = 1:length(l),
      ll = l
    ) %>%
    unnest(ll)
  
  
  return(dd)
  
}



## para caret , confusionMatrix

tabla_metricas = function(MatrizConfusion, metodo){
  
  cf = MatrizConfusion
  
  cf$overall %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    dplyr::rename(
      "metricas" = 1,
      "valor" = 2
    ) %>% 
    bind_rows(
      cf$byClass %>% 
        as.data.frame() %>% 
        rownames_to_column() %>% 
        dplyr::rename(
          "metricas" = 1,
          "valor" = 2
        )
    ) %>% 
    kable(booktabs =T,
          caption = glue::glue("Métricas del metodo: {metodo} "),
          label = glue::glue("metricas_{metodo}"),
          align = "c") %>%
    kable_styling(latex_options = c("striped", "hold_position")) %>%
    row_spec(0, bold = T, color = "white", background = "black", align = "c")
  
}


tabla_confusionMatrix = function(MatrizConfusion, metodo){
  
  cf = MatrizConfusion
  
  cf$table %>% 
    kable(booktabs =T,
          caption = glue::glue("Matriz de Confusion del metodo: {metodo} "),
          label = glue::glue("MatrizConf_{metodo}"),
          align = "c") %>%
    kable_styling(latex_options = c("striped", "hold_position")) %>%
    row_spec(0, bold = T, color = "white", background = "black", align = "c") %>% 
    kableExtra::add_header_above(c("Prediccion","Referencia"," "))
  
}




