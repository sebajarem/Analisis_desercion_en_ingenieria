###############################################################################
# MODELO: NAIVE BAYES
###############################################################################

# Empleando el teorema de Bayes, el algoritmo de Naive Bayes calcula las probabilidades condicionales de que una observación pertenezca a cada una de las clases dadas unas evidencias (valores de los predictores). El término naive (ingenuo en inglés) se debe a que el algoritmo asume que las variables son independientes, es decir, que el valor que toman no está influenciado por las demás. Aunque en la práctica, esta asunción raramente es cierta, permite calcular la probabilidad cuando hay múltiples predictores simplemente multiplicando las probabilidades individuales de cada uno (regla de eventos independientes). A pesar de que es solo una aproximación, puede resultar muy efectiva.

library(lubridate)
utils::capture.output(paste(lubridate::now() - lubridate::hours(3)),
                      file = paste0(getwd(),"/", config$outputs, "modelo_naivebayes.txt"),
                      append = TRUE,
                      type = c("output"))

# El método nb de caret emplea la función NaiveBayes() del paquete klaR con tres hiperparámetros:
# usekernel: TRUE para emplear un kernel que estime la densidad o FALSE para asumir una distribución de densidad gaussiana.
# fL: factor de corrección de Laplace, 0 para no aplicar ninguna corrección.
# adjust: parámetro pasado a la función density si usekernel = TRUE.

# PARALELIZACIÓN DE PROCESO
#===============================================================================
library(doMC)
registerDoMC(cores = 3)

# HIPERPARÁMETROS, NÚMERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICIÓN
#===============================================================================
particiones  <- 10
repeticiones <- 5

# Hiperparámetros
hiperparametros <- data.frame(usekernel = FALSE, fL = 0 , adjust = 0)

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
modelo_nb <- train(deserto ~ ., data = datos_train_prep,
                   method = "nb",
                   tuneGrid = hiperparametros,
                   metric = "Accuracy",
                   trControl = control_train)

saveRDS(modelo_nb, paste0(getwd(),"/",config$outputs,"modelo_nb.rds"))

# RESULTADOS
# ==============================================================================
modelo_nb

utils::capture.output(paste(lubridate::now() - lubridate::hours(3)),
                      file = paste0(getwd(),"/", config$outputs, "modelo_naivebayes.txt"),
                      append = TRUE,
                      type = c("output"))

utils::capture.output(modelo_nb,
                      file = paste0(getwd(),"/", config$outputs, "modelo_naivebayes.txt"),
                      append = TRUE,
                      type = c("output"))








