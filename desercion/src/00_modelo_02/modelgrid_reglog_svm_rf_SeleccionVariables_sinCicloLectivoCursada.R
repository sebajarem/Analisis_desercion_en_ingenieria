###############################################################################
# MODELO: MODEL GRID
###############################################################################
# con seleccion de varibles y sin la mas importante
###############################################################################

ArchivoDeSalida = "modelos_reglog_svm_rf_datos02.txt"
ArchivoDeSalidaRDS = "modelos_reglog_svm_rf_datos02.rds"

#  Muchos modelos simultaneamente

# El objeto model_grid contiene 3 elementos, en los que se almacenan 3 tipos de información:
# shared_settings: almacena información de configuración que se aplica de la misma forma a todos los modelos que forman el grid. Generalmente suele ser el nombre de la variable respuesta y de los predictores, el tipo de estrategia de validación, transformaciones de preprocesado, etc.
# models: almacena el nombre de los modelos que forman el grid, así como elementos de configuración individuales. Si un mismo parámetro se encuentra definido tanto en $shared_settings como en el modelo individual, este último toma preferencia.
# model_fits: almacena los modelos ajustados (uno por cada modelo definido en models) después de entrenar el model_grid.

library(lubridate)
utils::capture.output(paste(lubridate::now() - lubridate::hours(3)),
                      file = paste0(getwd(),"/", config$outputs, ArchivoDeSalida),
                      append = TRUE,
                      type = c("output"))

# CREACION DEL GRID
#==============================================================================
library(modelgrid)

grid_modelos <- model_grid()
grid_modelos

utils::capture.output(grid_modelos,
                      file = paste0(getwd(),"/", config$outputs, ArchivoDeSalida),
                      append = TRUE,
                      type = c("output"))

# DIFINICION SHARED SETTINGS
#==============================================================================

grid_modelos <- grid_modelos %>%
  share_settings(
    y = datos_train_prep$deserto,
    # x = datos_train_prep %>% select(-deserto), # todo el dataset
    x = datos_train_prep %>% select(-deserto) %>% select(rf_rfe$optVariables) %>% select(-ciclo_lectivo_de_cursada), # seleccion variables por rf
    # x = datos_train_prep %>% select(-deserto) %>% select(), # seleccion varuiables por gbm
    metric = "Accuracy",
    trControl = trainControl(method = "repeatedcv",
                             number = 5,
                             repeats = 8,
                             returnResamp = "final",
                             verboseIter = FALSE,
                             allowParallel = TRUE
    )
  )

# CONFIGURACIONES INDIVIDUALES DE CADA MODELO
#==============================================================================

grid_modelos <- grid_modelos %>%
  add_model(
    model_name = "Reg_logistica",
    method     = "glm",
    family     = binomial(link = "logit")
  ) %>%
  add_model(
    model_name = "RandomForest",
    method     = "ranger",
    num.trees  = 500,
    tuneGrid   = expand.grid(
      mtry = c(3, 4, 5, 7),
      min.node.size = c(2, 3, 4, 5, 10, 15, 20, 30),
      splitrule = "gini"
    )
  ) %>%
  add_model(
    model_name = "SVM",
    method = "svmRadial",
    tuneGrid   = expand.grid(
      sigma = c(0.001, 0.01, 0.1, 0.5, 1),
      C = c(1 , 20, 50, 100, 200, 500, 700)
    )
  )
grid_modelos$models

utils::capture.output(grid_modelos$models,
                      file = paste0(getwd(),"/", config$outputs, ArchivoDeSalida),
                      append = TRUE,
                      type = c("output"))


# ENTRENAMIENTO
#==============================================================================

# Con la función train() se entrenan uno a uno los modelos del model_grid (internamente, se llama a la función train() de caret) y los resultados se almacenan en model_fits. Esta función tiene únicamente 3 argumentos:
# mg: el objeto model_grid que se va a entrenar.
# train_all: por defecto, solo se entrenan aquellos modelos que no han sido ajustados anteriormente, es decir, los que no tienen un objeto correspondiente en $model_fits. Esto es muy útil ya que permite incluir nuevos modelos en el grid sin necesidad de reentrenar los anteriores. Si se indica train_all = TRUE, todos los modelos se ajustan de nuevo.
# resample_seed: semilla que permite hacer reproducibles (y comparables) las particiones de validación en todos los modelos

# Se emplean 4 cores en paralelo.
library(doMC)
registerDoMC(cores = 3)

grid_modelos <- train(grid_modelos, train_all = FALSE, resample_seed = 123)

saveRDS(grid_modelos, paste0(getwd(),"/",config$outputs, ArchivoDeSalidaRDS))

# RESULTADOS
# ==============================================================================
grid_modelos$model_fits

library(lubridate)
utils::capture.output(paste(lubridate::now() - lubridate::hours(3)),
                      file = paste0(getwd(),"/", config$outputs, ArchivoDeSalida),
                      append = TRUE,
                      type = c("output"))


utils::capture.output(grid_modelos$model_fits,
                      file = paste0(getwd(),"/", config$outputs, ArchivoDeSalida),
                      append = TRUE,
                      type = c("output"))