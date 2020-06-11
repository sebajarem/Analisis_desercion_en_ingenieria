###############################################################################
# MODELO: Gradient Boosting
###############################################################################

# El método gbm de caret emplea la función gbm() del paquete gbm. Este algoritmo tiene 4 hiperparámetros:
# n.trees: número de iteraciones del algoritmo de boosting, es decir, número de modelos que forman el ensemble. Cuanto mayor es este valor, más se reduce el error de entrenamiento, pudiendo llegar generarse overfitting.
# interaction.depth: complejidad de los árboles empleados como weak learner, en concreto, el número total de divisiones que tiene el árbol. Emplear árboles con ente 1 y 6 nodos suele dar buenos resultados.
# shrinkage: este parámetro, también conocido como learning rate, controla la influencia que tiene cada modelo sobre el conjunto del ensemble. Es preferible mejorar un modelo mediante muchos pasos pequeños que mediante unos pocos grandes. Por esta razón, se recomienda emplear un valor de shrinkage tan pequeño como sea posible, teniendo en cuenta que, cuanto menor sea, mayor el número de iteraciones necesarias. Por defecto es de 0.001.
# n.minobsinnode: número mínimo de observaciones que debe tener un nodo para poder ser dividido. Al igual que interaction.depth, permite controlar la complejidad de los weak learners basados en árboles.
# 
# Además de los hiperparámetros que permite controlar caret, la función gbm() tiene otros dos más que hay que tener en cuenta:
# distribution: determina la función de coste (loss function). Algunas de las más utilizadas son: gaussian (squared loss) para regresión, bernoulli para respuestas binarias, multinomial para variables respuesta con más de dos clases y adaboost para respuestas binarias y que emplea la función exponencial del algoritmo original AdaBoost.
# bag.fraction (subsampling fraction): fracción de observaciones del set de entrenamiento seleccionadas de forma aleatoria para ajustar cada weak learner. Si su valor es de 1, se emplea el algoritmo de Gradient Boosting, si es menor que 1, se emplea Stochastic Gradient Boosting. Por defecto su valor es de 0.5.

library(lubridate)
utils::capture.output(paste(lubridate::now() - lubridate::hours(3)),
                      file = paste0(getwd(),"/", config$outputs, "modelo_boost.txt"),
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
hiperparametros <- expand.grid(interaction.depth = c(1, 5, 9),
                               n.trees = c(100, 500, 1000, 2000),
                               shrinkage = c(0.001, 0.01, 0.1),
                               n.minobsinnode = c(2, 10, 20))

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
modelo_boost <- train(deserto ~ ., data = datos_train_prep,
                      method = "gbm",
                      tuneGrid = hiperparametros,
                      metric = "Accuracy",
                      trControl = control_train,
                      # Número de árboles ajustados
                      distribution = "adaboost",
                      verbose = FALSE)

saveRDS(modelo_boost, paste0(getwd(),"/",config$outputs,"modelo_boost.rds"))

# RESULTADOS
# ==============================================================================
modelo_boost

utils::capture.output(paste(lubridate::now() - lubridate::hours(3)),
                      file = paste0(getwd(),"/", config$outputs, "modelo_boost.txt"),
                      append = TRUE,
                      type = c("output"))

utils::capture.output(modelo_boost,
                      file = paste0(getwd(),"/", config$outputs, "modelo_boost.txt"),
                      append = TRUE,
                      type = c("output"))

# # REPRESENTACIÓN GRÁFICA
# # ==============================================================================
# ggplot(modelo_boost, highlight = TRUE) +
#   labs(title = "Evolución del accuracy del modelo Gradient Boosting") +
#   guides(color = guide_legend(title = "shrinkage"),
#          shape = guide_legend(title = "shrinkage")) +
#   theme_bw() +
#   theme(legend.position = "bottom")



