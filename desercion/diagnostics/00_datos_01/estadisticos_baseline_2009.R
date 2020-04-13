

baseline <- read_csv("./outputs/00_datos_01/baseline_2009.csv")

# t(baseline[1:3,]) %>%
  
baseline_sample = 
  baseline[1:3,] %>%
  gather(key = variable,
         value = valor,
         -`Codigo Alumno`) %>%
  select(
    codigo.alumno = `Codigo Alumno`,
    everything()
  ) %>%
  mutate(
    codigo.alumno = as.character(codigo.alumno)
  ) %>%
  spread(key = codigo.alumno,
         value = valor)


tablalarga2latex(df = baseline_sample,
            archivo_salida = "baseline_sample.tex",
            caption_tabla = "Ejemplo de Tablón",
            nombre_data = "tablon_baseline")



mtcars %>%
  rownames_to_column %>% 
  gather(var, value, -rowname) %>% 
  spread(rowname, value) 
In the even newer version, pivot_wider replaces spread:
  
  mtcars %>%
  rownames_to_column %>% 
  gather(var, value, -rowname) %>% 
  pivot_wider(names_from=rowname, values_from=value) 