###############################################################################
# MODELO: REGRESION LOGISTICA
###############################################################################

# El método glm de caret emplea la función glm() del paquete básico de R. Este algoritmo no tiene ningún hiperparámetro pero, para que efectúe una regresión logística, hay que indicar family = "binomial".

library(lubridate)
utils::capture.output(paste(lubridate::now() - lubridate::hours(3)),
                      file = paste0(getwd(),"/", config$outputs, "modelo_reglogistica.txt"),
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
modelo_logistic <- train(deserto ~ ., data = datos_train_prep,
                         method = "glm",
                         tuneGrid = hiperparametros,
                         metric = "Accuracy",
                         trControl = control_train,
                         family = "binomial")

saveRDS(modelo_logistic, paste0(getwd(),"/",config$outputs,"modelo_logistic.rds"))

# RESULTADOS
# ==============================================================================
modelo_logistic

utils::capture.output(paste(lubridate::now() - lubridate::hours(3)),
                      file = paste0(getwd(),"/", config$outputs, "modelo_reglogistica.txt"),
                      append = TRUE,
                      type = c("output"))

utils::capture.output(modelo_logistic,
                      file = paste0(getwd(),"/", config$outputs, "modelo_reglogistica.txt"),
                      append = TRUE,
                      type = c("output"))

utils::capture.output(summary(modelo_logistic$finalModel),
                      file = paste0(getwd(),"/", config$outputs, "modelo_reglogistica.txt"),
                      append = TRUE,
                      type = c("output"))

