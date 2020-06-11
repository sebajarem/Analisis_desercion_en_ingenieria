###############################################################################
# MODELO: RandomForest
###############################################################################

# El método ranger de caret emplea la función ranger() del paquete ranger. Este algoritmo tiene 3 hiperparámetros:
# mtry: número predictores seleccionados aleatoriamente en cada árbol.
# min.node.size: tamaño mínimo que tiene que tener un nodo para poder ser dividido.
# splitrule: criterio de división.
# Aunque caret también incluye el método rf con la función rf() del paquete randomForest, este último solo permite optimizar el hiperparámetro mtry.

library(lubridate)
utils::capture.output(paste(lubridate::now() - lubridate::hours(3)),
                      file = paste0(getwd(),"/", config$outputs, "modelo_rf.txt"),
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
hiperparametros <- expand.grid(mtry = c(3, 4, 5, 7),
                               min.node.size = c(2, 3, 4, 5, 10, 15, 20, 30),
                               splitrule = "gini")

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
modelo_rf <- train(deserto ~ ., data = datos_train_prep,
                   method = "ranger",
                   tuneGrid = hiperparametros,
                   metric = "Accuracy",
                   trControl = control_train,
                   # Número de árboles ajustados
                   num.trees = 500)

saveRDS(modelo_rf, paste0(getwd(),"/",config$outputs,"modelo_rf.rds"))

# RESULTADOS
# ==============================================================================
modelo_rf

utils::capture.output(paste(lubridate::now() - lubridate::hours(3)),
                      file = paste0(getwd(),"/", config$outputs, "modelo_rf.txt"),
                      append = TRUE,
                      type = c("output"))

utils::capture.output(modelo_rf,
                      file = paste0(getwd(),"/", config$outputs, "modelo_rf.txt"),
                      append = TRUE,
                      type = c("output"))

utils::capture.output(modelo_rf$finalModel,
                      file = paste0(getwd(),"/", config$outputs, "modelo_rf.txt"),
                      append = TRUE,
                      type = c("output"))

# # REPRESENTACIÓN GRÁFICA
# # ==============================================================================
# ggplot(modelo_rf, highlight = TRUE) +
#   scale_x_continuous(breaks = 1:30) +
#   labs(title = "Evolución del accuracy del modelo Random Forest") +
#   guides(color = guide_legend(title = "mtry"),
#          shape = guide_legend(title = "mtry")) +
#   theme_bw()




