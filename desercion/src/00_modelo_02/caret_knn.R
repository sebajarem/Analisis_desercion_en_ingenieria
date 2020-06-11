###############################################################################
# MODELO: KNN
###############################################################################

# K-Nearest Neighbor es un algoritmo simple. Identificar observaciones en el conjunto de entrenamiento que se asemejen a la observación de test (observaciones vecinas) y asignarle como valor predicho la clase predominante entre dichas observaciones.
library(lubridate)
utils::capture.output(paste(lubridate::now() - lubridate::hours(3)),
               file = paste0(getwd(),"/", config$outputs, "modelo_knn.txt"),
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
hiperparametros <- data.frame(k = c(1, 2, 5, 10, 15, 20, 30, 50, 60, 70, 80))

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
modelo_knn <- train(deserto ~ ., data = datos_train_prep,
                    method = "knn",
                    tuneGrid = hiperparametros,
                    metric = "Accuracy",
                    trControl = control_train)

saveRDS(modelo_knn, paste0(getwd(),"/",config$outputs,"modelo_knn.rds"))

# # RESULTADOS
# # ==============================================================================
modelo_knn

utils::capture.output(paste(lubridate::now() - lubridate::hours(3)),
                      file = paste0(getwd(),"/", config$outputs, "modelo_knn.txt"),
                      append = TRUE,
                      type = c("output"))

utils::capture.output(modelo_knn,
                      file = paste0(getwd(),"/", config$outputs, "modelo_knn.txt"),
                      append = TRUE,
                      type = c("output"))



# # REPRESENTACIÓN GRÁFICA
# # ==============================================================================
# ggplot(modelo_knn, highlight = TRUE) +
#   scale_x_continuous(breaks = hiperparametros$k) +
#   labs(title = "Evolución del accuracy del modelo KNN", x = "K") +
#   theme_bw()

