###############################################################################
# MODELO: SVM
###############################################################################
# sin variable ciclo lectivo
###############################################################################

ArchivoDeSalida = "modelos_svm_datos_07.txt"
ArchivoDeSalidaRDS = "modelos_svm_datos_07.rds"


# El método svmRadial de caret emplea la función ksvm() del paquete kernlab. Este algoritmo tiene 2 hiperparámetros:
# sigma: coeficiente del kernel radial.
# C: penalización por violaciones del margen del hiperplano.


library(lubridate)
utils::capture.output(paste(lubridate::now() - lubridate::hours(3)),
                      file = paste0(getwd(),"/", config$outputs, ArchivoDeSalida),
                      append = TRUE,
                      type = c("output"))


# PARALELIZACIÓN DE PROCESO
#===============================================================================
library(doMC)
registerDoMC(cores = 3)

# HIPERPARÁMETROS, NÚMERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN
#===============================================================================
particiones  <- 10
repeticiones <- 5

# Hiperparámetros
hiperparametros <- expand.grid(sigma = c(0.001, 0.01, 0.1, 0.5, 1),
                               C = c(1 , 20, 50, 100, 200, 500, 700))

set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICIÓN DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================
set.seed(342)
modelo_svmrad <- train(deserto ~ ., data = datos_train_prep %>% dplyr::select(-ciclo_lectivo_de_cursada),
                       method = "svmRadial",
                       tuneGrid = hiperparametros,
                       metric = "Accuracy",
                       trControl = control_train)

saveRDS(modelo_svmrad, paste0(getwd(),"/",config$outputs, ArchivoDeSalidaRDS))

# RESULTADOS
# ==============================================================================
modelo_svmrad

utils::capture.output(paste(lubridate::now() - lubridate::hours(3)),
                      file = paste0(getwd(),"/", config$outputs, ArchivoDeSalida),
                      append = TRUE,
                      type = c("output"))

utils::capture.output(modelo_svmrad,
                      file = paste0(getwd(),"/", config$outputs, ArchivoDeSalida),
                      append = TRUE,
                      type = c("output"))

utils::capture.output(modelo_svmrad$finalModel,
                      file = paste0(getwd(),"/", config$outputs, ArchivoDeSalida),
                      append = TRUE,
                      type = c("output"))

# # REPRESENTACIÓN GRÁFICA
# # ==============================================================================
# ggplot(modelo_svmrad, highlight = TRUE) +
#   labs(title = "Evolución del accuracy del modelo SVM Radial") +
#   theme_bw()
