###############################################################################
# MODELO: REDES NEURONALES - NNET
###############################################################################

# El método nnet de caret emplea la función nnet() del paquete nnet para crear redes neuronales con una capa oculta. Este algoritmo tiene 2 hiperparámetros:
# size: número de neuronas en la capa oculta.
# decay: controla la regularización durante el entrenamiento de la red.
# Además de estos hiperparámetros, la función nnet() tiene muchos otros argumentos que controlan el proceso de aprendizaje de la red. Entre ellos cabe destacar MaxNWts, que determina el número máximo de pesos. Por defecto, su valor es 1000, pero puede ocurrir que, si la red tiene muchas neuronas, se necesiten más pesos.


library(lubridate)
utils::capture.output(paste(lubridate::now() - lubridate::hours(3)),
                      file = paste0(getwd(),"/", config$outputs, "modelo_rna_nnet.txt"),
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
hiperparametros <- expand.grid(size = c(10, 20, 50, 80, 100, 120),
                               decay = c(0.0001, 0.1, 0.5))

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
modelo_nnet <- train(deserto ~ ., data = datos_train_prep,
                     method = "nnet",
                     tuneGrid = hiperparametros,
                     metric = "Accuracy",
                     trControl = control_train,
                     # Rango de inicialización de los pesos
                     rang = c(-0.7, 0.7),
                     # Número máximo de pesos
                     # se aumenta para poder incluir más meuronas
                     MaxNWts = 2000,
                     # Para que no se muestre cada iteración por pantalla
                     trace = FALSE)

saveRDS(modelo_nnet, paste0(getwd(),"/",config$outputs,"modelo_nnet.rds"))

# RESULTADOS
# ==============================================================================
modelo_nnet


utils::capture.output(paste(lubridate::now() - lubridate::hours(3)),
                      file = paste0(getwd(),"/", config$outputs, "modelo_rna_nnet.txt"),
                      append = TRUE,
                      type = c("output"))

utils::capture.output(modelo_nnet,
                      file = paste0(getwd(),"/", config$outputs, "modelo_rna_nnet.txt"),
                      append = TRUE,
                      type = c("output"))


# # REPRESENTACIÓN GRÁFICA
# # ==============================================================================
# ggplot(modelo_nnet, highlight = TRUE) +
#   labs(title = "Evolución del accuracy del modelo NNET") +
#   theme_bw()

