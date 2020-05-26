###############################################################################
# MODELO: LDA - lineal discriminant analisis
###############################################################################

# El método lda de caret emplea la función lda() del paquete MASS. Este algoritmo no tiene ningún hiperparámetro.

library(lubridate)
utils::capture.output(paste(lubridate::now() - lubridate::hours(3)),
                      file = paste0(getwd(),"/", config$outputs, "modelo_lda.txt"),
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
hiperparametros <- data.frame(parameter = "none")

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
modelo_lda <- train(deserto ~ ., data = datos_train_prep,
                    method = "lda",
                    tuneGrid = hiperparametros,
                    metric = "Accuracy",
                    trControl = control_train)

saveRDS(modelo_lda, paste0(getwd(),"/",config$outputs,"modelo_lda.rds"))

# RESULTADOS
# ==============================================================================
modelo_lda
modelo_lda$finalModel

utils::capture.output(paste(lubridate::now() - lubridate::hours(3)),
                      file = paste0(getwd(),"/", config$outputs, "modelo_lda.txt"),
                      append = TRUE,
                      type = c("output"))

utils::capture.output(modelo_lda,
                      file = paste0(getwd(),"/", config$outputs, "modelo_lda.txt"),
                      append = TRUE,
                      type = c("output"))

utils::capture.output(modelo_lda$finalModel,
                      file = paste0(getwd(),"/", config$outputs, "modelo_lda.txt"),
                      append = TRUE,
                      type = c("output"))

