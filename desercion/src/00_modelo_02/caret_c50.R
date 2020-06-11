###############################################################################
# MODELO: C5.0 - Arbol de decisión
###############################################################################



library(lubridate)
utils::capture.output(paste(lubridate::now() - lubridate::hours(3)),
                      file = paste0(getwd(),"/", config$outputs, "modelo_c50.txt"),
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
modelo_C50Tree <- train(deserto ~ ., data = datos_train_prep,
                        method = "C5.0Tree",
                        tuneGrid = hiperparametros,
                        metric = "Accuracy",
                        trControl = control_train)

saveRDS(modelo_C50Tree, paste0(getwd(),"/",config$outputs,"modelo_C50Tree.rds"))

# RESULTADOS
# ==============================================================================
modelo_C50Tree

utils::capture.output(paste(lubridate::now() - lubridate::hours(3)),
                      file = paste0(getwd(),"/", config$outputs, "modelo_c50.txt"),
                      append = TRUE,
                      type = c("output"))

utils::capture.output(modelo_C50Tree,
                      file = paste0(getwd(),"/", config$outputs, "modelo_c50.txt"),
                      append = TRUE,
                      type = c("output"))

utils::capture.output(summary(modelo_C50Tree$finalModel),
                      file = paste0(getwd(),"/", config$outputs, "modelo_c50.txt"),
                      append = TRUE,
                      type = c("output"))




