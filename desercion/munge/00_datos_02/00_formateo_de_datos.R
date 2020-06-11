# en este script se va a hacer:
# 1 - imputar los valores ausentes de la variable EsTecnico por un variable nuev
# 2 - convertir los tipos de datos en factor
# dividir el dataset en train y test

datos = baseline

glimpse(datos)

###############################################################################
# VALORES AUSENTES
###############################################################################


map_dbl(datos, .f = function(x){sum(is.na(x))})

datos %>% map_lgl(.f = function(x){any(!is.na(x) & x == "")})

# los unicos resgitros ausentes estan en la variable EsTecnico

datos =
  datos %>% 
  mutate(EsTecnico = replace(EsTecnico, is.na(EsTecnico), "SinDato"))

###############################################################################
# formateo de datos
###############################################################################

# paso a factor los character
datos =
  datos %>% 
  mutate_if(is.character, as.factor)


###############################################################################
# Analisis de varianza cercana a cero
###############################################################################
library(caret)
datos %>% select_if(is.numeric) %>%
  nearZeroVar(saveMetrics = TRUE)

datos =
  datos %>% 
  mutate(
    cant_recursada_regular_RecursoMveces = cant_recursada_regular_RecursoNveces +
                                               cant_recursada_regular_Recurso5vez
  )

# ###############################################################################
# # Importancia de variables
# ###############################################################################
# 
# library(randomForest)
# 
# modelo_randforest <- randomForest(formula = deserto ~ . ,
#                                   data = datos %>% select(-codigo.alumno),
#                                   mtry = 5,
#                                   importance = TRUE, 
#                                   ntree = 1000)
# 
# importancia <- as.data.frame(modelo_randforest$importance)
# importancia <- rownames_to_column(importancia,var = "variable")
# 
# library(ggpubr)
# 
# 
# p1 <- ggplot(data = importancia, aes(x = reorder(variable, MeanDecreaseAccuracy),
#                                      y = MeanDecreaseAccuracy,
#                                      fill = MeanDecreaseAccuracy)) +
#   labs(x = "variable", title = "Reducción de Accuracy") +
#   geom_col() +
#   coord_flip() +
#   theme_bw() +
#   theme(legend.position = "bottom")
# 
# p2 <- ggplot(data = importancia, aes(x = reorder(variable, MeanDecreaseGini),
#                                      y = MeanDecreaseGini,
#                                      fill = MeanDecreaseGini)) +
#   labs(x = "variable", title = "Reducción de pureza (Gini)") +
#   geom_col() +
#   coord_flip() +
#   theme_bw() +
#   theme(legend.position = "bottom")
# ggarrange(p1, p2)

###############################################################################
# Train Test
###############################################################################

set.seed(123)
# Se crean los índices de las observaciones de entrenamiento
train <- createDataPartition(y = datos$deserto, p = 0.7, list = FALSE, times = 1)
datos_train <- datos[train, ]
datos_test  <- datos[-train, ]

###############################################################################
# Guradado de datos
###############################################################################
getwd()
config$outputs

saveRDS(datos_train, paste0(getwd(),"/",config$outputs,"baseline_2009_train.rds") )
saveRDS(datos_test, paste0(getwd(),"/",config$outputs,"baseline_2009_test.rds") )

round(prop.table(table(datos_train$deserto)),2)
round(prop.table(table(datos_test$deserto)),2)



