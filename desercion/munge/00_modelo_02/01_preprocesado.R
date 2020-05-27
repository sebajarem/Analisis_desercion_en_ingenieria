###############################################################################
# Preprocesado
###############################################################################

#==============================================================================
# Objeto Recipe
#==============================================================================
library(recipes)
# Se crea un objeto recipe() con la variable respuesta y los predictores. 

objeto_recipe <- recipe(formula = deserto ~ .,
                        data =  datos_train %>% select(-codigo.alumno))

objeto_recipe
#==============================================================================
# imputacion
#==============================================================================

#==============================================================================
# eliminacion variables por varianza proxima a cero
#==============================================================================
objeto_recipe <- objeto_recipe %>% step_nzv(all_predictors())

#==============================================================================
# estandarizacion y centrado
#==============================================================================
objeto_recipe <- objeto_recipe %>% step_center(all_numeric())
objeto_recipe <- objeto_recipe %>% step_scale(all_numeric())

#==============================================================================
# Binarizacion de variables cualitativas
#==============================================================================
objeto_recipe <- objeto_recipe %>% step_dummy(all_nominal(), -all_outcomes())


#==============================================================================
# Entrenamiento y aplicacion de preprocesado a datastets
#==============================================================================
# Se entrena el objeto recipe
trained_recipe <- prep(objeto_recipe, training = datos_train)
trained_recipe

# Se aplican las transformaciones al conjunto de entrenamiento y de test
datos_train_prep <- bake(trained_recipe, new_data = datos_train)
datos_test_prep  <- bake(trained_recipe, new_data = datos_test)

saveRDS(datos_train_prep, paste0(getwd(),"/",config$outputs,"baseline_2009_train_prep.rds") )
saveRDS(datos_test_prep, paste0(getwd(),"/",config$outputs,"baseline_2009_test_prep.rds") )

glimpse(datos_train_prep)
glimpse(datos_test_prep)
