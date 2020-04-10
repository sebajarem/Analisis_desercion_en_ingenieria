if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
if (!require(dlookr)) install.packages('dlookr')
library(dlookr)
if (!require(openxlsx)) install.packages('openxlsx')
library(openxlsx)


# diagnóstico de la calidad de los datos
# nobs, nulos y únicos
estadisticos_calidad <- function(datos, variables_excluir = NA, generar_xlsx = T, nombre_xlsx = "estadisticos(nobs-nulos-unicos)") {
  
  datos <- datos %>% select(-variables_excluir)
  
  a <- dlookr::diagnose(datos)

  if(is.null(a)) {
    message("No hay datos")
    return(NULL)
  }
  
  # cambia nombres de las columnas
  names(a) <- c("variable", "tipo", "nulos", "nulos_pct", "valores_unicos", "valores_unicos_pct")
  
  # calcula nuevas variables y ordena el dataset
  a <-
    a %>%
    mutate(
      "observaciones" = nrow(datos) - a$nulos,
      "observaciones_pct" = 100 - a$nulos_pct,
      "valores_unicos_pct" = a$valores_unicos_pct *100
    ) %>%
    select(c("variable", "tipo", "observaciones", "observaciones_pct", "nulos", "nulos_pct", "valores_unicos", "valores_unicos_pct"))

  # redondeo a 2 dígitos
  a <- a %>% mutate_if(is.numeric, round, digits = 2)
  
  if(generar_xlsx == T) {
    # marcador de miles "." y decimales ","
    a_xlsx <- a %>% mutate_if(is.numeric, format, big.mark = ".", decimal.mark = ",", scientific = F, drop0trailing = T, trim  = T)
    # Escribe nulo en los NA para que se muestren
    a_xlsx[is.na(a_xlsx)] <- "< nulo >"
    # se agrega % a las columnas pertinentes
    a_xlsx <-
      a_xlsx %>%
      mutate(
        "observaciones_pct" = str_c(a_xlsx$observaciones_pct, "%"),
        "nulos_pct" = str_c(a_xlsx$nulos_pct, "%"),
        "valores_unicos_pct" = str_c(a_xlsx$valores_unicos_pct, "%")
      )
    # salida en xlsx
    wb <- createWorkbook()
    sheet <- "estadisticos(nobs-nulos-unicos)"
    addWorksheet(wb, sheet)
    writeData(wb, sheet, format(Sys.time(), "%d/%m/%Y"), startCol = 1 , rowNames = F)  # se agrega una columna con la fecha de creación del archivo
    writeData(wb, sheet, a_xlsx, startCol = 2 , rowNames = F)
    nombre_xlsx <- str_replace(nombre_xlsx, ".xlsx$", "")
    saveWorkbook(wb, str_c(nombre_xlsx, format(Sys.time(), "_%Y%m%d"), ".xlsx"), overwrite = TRUE)
  }
  
  return(a)
}


# diagnóstico de los datos numéricos
# estadísticos - numéricos
estadisticos_numericos <- function(datos, variables_excluir = NA, generar_xlsx = T, nombre_xlsx = "estadisticos(numericos)") {
  
  datos <- datos %>% select(-variables_excluir)
  
  b <- dlookr::diagnose_numeric(datos)
  
  if(is.null(b)) {
    message("No hay datos numéricos")
    return(NULL)
  }
  
  # cambia nombres de las columnas
  names(b) <- c("variable", "minimo", "Q1", "promedio", "mediana", "Q3", "maximo", "ceros", "negativos", "outliers")
  
  # calcula nuevas variables y ordena el dataset
  b <-
    b %>%
    mutate(
      "P05" = unname(apply(datos %>% select_if(is.numeric), MARGIN = 2, FUN = quantile, probs = c(.05) , na.rm = T)),
      "P95" = unname(apply(datos %>% select_if(is.numeric), MARGIN = 2, FUN = quantile, probs = c(.95) , na.rm = T)),
      "desvío" = unname(apply(datos %>% select_if(is.numeric), MARGIN = 2, FUN = sd, na.rm = T)),
      "ceros_pct" = b$ceros *100 /nrow(datos),
      "negativos_pct" = b$negativos *100 /nrow(datos),
      "outliers_pct" = b$outliers *100 /nrow(datos)
    ) %>%
    select(c("variable", "promedio", "desvío", "minimo", "maximo", "P05", "Q1", "mediana", "Q3", "P95", "ceros", "ceros_pct", "negativos", "negativos_pct", "outliers", "outliers_pct"))
  
  # redondeo a 2 dígitos
  b <- b %>% mutate_if(is.numeric, round, digits = 2)
  
  if(generar_xlsx == T) {
    # marcador de miles "." y decimales ","
    b_xlsx <- b %>% mutate_if(is.numeric, format, big.mark = ".", decimal.mark = ",", scientific = F, drop0trailing = T, trim  = T)
    # Escribe nulo en los NA para que se muestren
    b_xlsx[is.na(b_xlsx)] <- "< nulo >"
    # se agrega % a las columnas pertinentes
    b_xlsx <-
      b_xlsx %>%
      mutate(
        "ceros_pct" = str_c(b_xlsx$ceros_pct, "%"),
        "negativos_pct" = str_c(b_xlsx$negativos_pct, "%"),
        "outliers_pct" = str_c(b_xlsx$outliers_pct, "%")
      )
    # salida en xlsx
    wb <- createWorkbook()
    sheet <- "estadisticos(numericos)"
    addWorksheet(wb, sheet)
    writeData(wb, sheet, format(Sys.time(), "%d/%m/%Y"), startCol = 1 , rowNames = F)  # se agrega una columna con la fecha de creación del archivo
    writeData(wb, sheet, b_xlsx, startCol = 2, rowNames = F)
    nombre_xlsx <- str_replace(nombre_xlsx, ".xlsx$", "")
    saveWorkbook(wb, str_c(nombre_xlsx, format(Sys.time(), "_%Y%m%d"), ".xlsx"), overwrite = TRUE)
  }
  
  return(b)
}


# diagnóstico de los datos categóricos
# top frecuencias - categóricos
estadisticos_categoricos <- function(datos, top = 10, variables_excluir = NA, generar_xlsx = T, nombre_xlsx = "estadisticos(categoricos)") {
  
  datos <- datos %>% select(-variables_excluir)
  
  datos <- datos %>% mutate_if(is.factor, as.character)
  
  c <- dlookr::diagnose_category(datos, top = top)
  
  if(is.null(c)) {
    message("No hay datos categóricos")
    return(NULL)
  }
  
  # borra estadisticos(columnas) que no se van a mostrar
  c <- c %>% select(-c("N"))
  
  # cambia nombres de las columnas
  names(c) <- c("variable", "característica", "frecuencia", "frecuencia_pct", "rank")
  
  # redondeo a 2 dígitos
  c <- c %>% mutate_if(is.numeric, round, digits = 2)
  
  if(generar_xlsx == T) {
    # marcador de miles "." y decimales ","
    c_xlsx <- c %>% mutate_if(is.numeric, format, big.mark = ".", decimal.mark = ",", scientific = F, drop0trailing = T, trim  = T)
    # Escribe nulo en los NA para que se muestren
    c_xlsx[is.na(c_xlsx)] <- "< nulo >"
    # se agrega % a las columnas pertinentes
    c_xlsx <-
      c_xlsx %>%
      mutate(
        "frecuencia_pct" = str_c(c_xlsx$frecuencia_pct, "%")
      )
    # salida en xlsx
    wb <- createWorkbook()
    sheet <- "estadisticos(categoricos)"
    addWorksheet(wb, sheet)
    writeData(wb, sheet, format(Sys.time(), "%d/%m/%Y"), startCol = 1 , rowNames = F)  # se agrega una columna con la fecha de creación del archivo
    writeData(wb, sheet, c_xlsx, startCol = 2, rowNames = F)
    nombre_xlsx <- str_replace(nombre_xlsx, ".xlsx$", "")
    saveWorkbook(wb, str_c(nombre_xlsx, format(Sys.time(), "_%Y%m%d"), ".xlsx"), overwrite = TRUE)
  }
  
  return(c)
}


# diagnóstico de la calidad de los datos, reune los distintos estadisticos del paquete en uno
# nobs, nulos y únicos
# estadísticos - numéricos
# top frecuencias - categóricos
estadisticos <- function(datos, top = 10, variables_excluir_calidad = NA, variables_excluir_numericos = NA, variables_excluir_categoricos = NA, generar_xlsx = T, nombre_xlsx = "estadisticos") {
  a <- estadisticos_calidad(datos, variables_excluir = variables_excluir_calidad, generar_xlsx = F)
  b <- estadisticos_numericos(datos, variables_excluir = variables_excluir_numericos, generar_xlsx = F)
  c <- estadisticos_categoricos(datos, top = top, variables_excluir = variables_excluir_categoricos, generar_xlsx = F)
  
  if(is.null(a) | is.null(b) | is.null(c)) {
    message("No se han podido generar los estadísticos")
    return(NULL)
  }
  
  if(generar_xlsx == T) {
    # marcador de miles "." y decimales ","
    a_xlsx <- a %>% mutate_if(is.numeric, format, big.mark = ".", decimal.mark = ",", scientific = F, drop0trailing = T, trim  = T)
    b_xlsx <- b %>% mutate_if(is.numeric, format, big.mark = ".", decimal.mark = ",", scientific = F, drop0trailing = T, trim  = T)
    c_xlsx <- c %>% mutate_if(is.numeric, format, big.mark = ".", decimal.mark = ",", scientific = F, drop0trailing = T, trim  = T)
    
    # Escribe nulo en los NA para que se muestren
    a_xlsx[is.na(a_xlsx)] <- "< nulo >"
    b_xlsx[is.na(b_xlsx)] <- "< nulo >"
    c_xlsx[is.na(c_xlsx)] <- "< nulo >"
    # se agrega % a las columnas pertinentes
    a_xlsx <-
      a_xlsx %>%
      mutate(
        "observaciones_pct" = str_c(a_xlsx$observaciones_pct, "%"),
        "nulos_pct" = str_c(a_xlsx$nulos_pct, "%"),
        "valores_unicos_pct" = str_c(a_xlsx$valores_unicos_pct, "%")
      )
    b_xlsx <-
      b_xlsx %>%
      mutate(
        "ceros_pct" = str_c(b_xlsx$ceros_pct, "%"),
        "negativos_pct" = str_c(b_xlsx$negativos_pct, "%"),
        "outliers_pct" = str_c(b_xlsx$outliers_pct, "%")
      )
    c_xlsx <-
      c_xlsx %>%
      mutate(
        "frecuencia_pct" = str_c(c_xlsx$frecuencia_pct, "%")
      )
    # salida en xlsx
    wb <- createWorkbook()
    sheeta <- "estadisticos(nobs-nulos-unicos)"
    sheetb <- "estadisticos(numericos)"
    sheetc <- "estadisticos(categoricos)"
    addWorksheet(wb, sheeta)
    addWorksheet(wb, sheetb)
    addWorksheet(wb, sheetc)
    writeData(wb, sheeta, format(Sys.time(), "%d/%m/%Y"), startCol = 1 , rowNames = F)
    writeData(wb, sheetb, format(Sys.time(), "%d/%m/%Y"), startCol = 1 , rowNames = F)
    writeData(wb, sheetc, format(Sys.time(), "%d/%m/%Y"), startCol = 1 , rowNames = F)
    writeData(wb, sheeta, a_xlsx, startCol = 2, rowNames = F)
    writeData(wb, sheetb, b_xlsx, startCol = 2, rowNames = F)
    writeData(wb, sheetc, c_xlsx, startCol = 2, rowNames = F)
    nombre_xlsx <- str_replace(nombre_xlsx, ".xlsx$", "")
    saveWorkbook(wb, str_c(nombre_xlsx, format(Sys.time(), "_%Y%m%d"), ".xlsx"), overwrite = TRUE)
  }
  
  return(list(calidad = a, numericos = b, categoricos = c))
}
