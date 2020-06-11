###############################################################################
# SELECCION DE VARIABLES
###############################################################################
library(caret)

# ELIMINACIÓN RECURSIVA MEDIANTE RANDOM FOREST Y BOOTSTRAPPING
# =============================================================================

# Se paraleliza el proceso para que sea más rápido. El número de cores debe 
# seleccionarse en función del ordenador que se está empleando.
library(doMC)
registerDoMC(cores = 3)

# Tamaño de los conjuntos de predictores analizados
subsets <- c(3:ncol(datos_train_prep)-1)

# Número de resamples para el proceso de bootstrapping
repeticiones <- 10

# Se crea una semilla para cada repetición de validación. Esto solo es necesario si
# se quiere asegurar la reproducibilidad de los resultados, ya que la validación
# cruzada y el bootstrapping implican selección aleatoria.

# El número de semillas necesarias depende del número total de repeticiones: 
# Se necesitan B+1 elementos donde B es el número total de particiones (CV) o
# resampling (bootstrapping). Los primeros B elementos deben ser vectores formados
# por M números enteros, donde M es el número de modelos ajustados, que en este caso
# se corresponde con el número de tamaños. El último elemento solo necesita un único
# número para ajustar el modelo final.
set.seed(123)
seeds <- vector(mode = "list", length = repeticiones + 1)
for (i in 1:repeticiones) {
  seeds[[i]] <- sample.int(1000, length(subsets))
} 
seeds[[repeticiones + 1]] <- sample.int(1000, 1)

# Se crea un control de entrenamiento donde se define el tipo de modelo empleado
# para la selección de variables, en este caso random forest, la estrategia de
# resampling, en este caso bootstrapping con 30 repeticiones, y las semillas para
# cada repetición. Con el argumento returnResamp = "all" se especifica que se
# almacene la información de todos los modelos generados en todas las repeticiones.
ctrl_rfe <- rfeControl(functions = rfFuncs, method = "boot", number = repeticiones,
                       returnResamp = "all", allowParallel = TRUE, verbose = FALSE,
                       seeds = seeds)

# Se ejecuta la eliminación recursiva de predictores
set.seed(342)
rf_rfe <- rfe(deserto ~ ., data = datos_train_prep,
              sizes = subsets,
              metric = "Accuracy",
              # El accuracy es la proporción de clasificaciones correctas
              rfeControl = ctrl_rfe,
              ntree = 500)
# Dentro de rfe() se pueden especificar argumentos para el modelo empleado, por
# ejemplo, el hiperparámetro ntree=500.

# Se muestra una tabla resumen con los resultados
rf_rfe

# El objeto rf_rfe almacena en optVariables las variables del mejor modelo.
rf_rfe$optVariables

# accuracy en todas las corridas
ggplot(data = rf_rfe$results, aes(x = Variables, y = Accuracy)) +
  geom_line() +
  scale_x_continuous(breaks  = unique(rf_rfe$results$Variables)) +
  geom_point() +
  geom_errorbar(aes(ymin = Accuracy - AccuracySD, ymax = Accuracy + AccuracySD),
                width = 0.2) +
  geom_point(data = rf_rfe$results %>% slice(which.max(Accuracy)),
             color = "red") +
  theme_bw()

# guardo objetos importantes
saveRDS(rf_rfe, paste0(getwd(),"/",config$outputs,"rf_rfe_modelo_seleccion_variables.rds"))



###############################################################################
# SELECCION DE VARIABLES
###############################################################################

# S-GBM
# =============================================================================

library(caret)

# PARALELIZACIÓN DE PROCESO
#===============================================================================
library(doMC)
registerDoMC(cores = 3)

# HIPERPARÁMETROS, NÚMERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN
#===============================================================================
particiones  <- 10
repeticiones <- 5

# Hiperparámetros
hiperparametros <-  expand.grid(interaction.depth = c(1, 5, 9), 
                            n.trees = c(100, 500, 1000, 2000), 
                            shrinkage = c(0.1, 0.01, 0.001),
                            n.minobsinnode = c(1, 10, 20))

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
set.seed(123)
modelo_boost <- train(deserto ~ ., data = datos_train_prep,
                      method = "gbm",
                      tuneGrid = hiperparametros,
                      metric = "Accuracy",
                      trControl = control_train,
                      # Número de árboles ajustados
                      distribution = "bernoulli", #xq es salida categorica
                      verbose = FALSE)
modelo_boost

# Se muestran los hiperparámetros del mejor modelo 
modelo_boost$bestTune

# variables importantes
#==============================================================================
importancia_pred <- summary(modelo_boost, plotit = FALSE)

importancia_pred =
  importancia_pred %>%
  mutate(
    rel.inf = round(rel.inf, 2)
  )

ggplot(data = importancia_pred, aes(x = reorder(var, rel.inf), y = rel.inf,
                                    fill = rel.inf)) +
  labs(x = "variable", title = "Reducción de MSE") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")


# guardo objetos importantes
saveRDS(modelo_boost, paste0(getwd(),"/",config$outputs,"gbm_modelo_seleccion_variables.rds"))


