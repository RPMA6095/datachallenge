library(readr)
library(dplyr)
library(mlogit)#multinomial regression
library(nnet)#multinomial regression
library(MASS)#akaike
library(stargazer)
library(caret)


#Lectura de datos
eco <- read_csv("ECO.csv") %>%
  as.data.frame()
educacion <- read_csv("EDUCACION.csv") %>%
  as.data.frame()
IDH <- read_csv("IDH.csv") %>%
  as.data.frame()
mun_entrenamiento <- read_csv("PRE_2018_MUNICIPIOS.csv") %>% 
  as.data.frame()

#Filtrado de "educación" por nivel educativo, año y columnas más importantes
edu_primaria <- filter(educacion, id_niveleducativo == 2, 
                       sostenimiento_educ == "Publico", ano == 2010) %>%
  dplyr::select(c(1:6,9,15,23))
  
edu_secundaria <- filter(educacion, id_niveleducativo == 3,
                         sostenimiento_educ == "Publico", ano == 2010) %>%
  dplyr::select(c(1:6,9,15,23))

edu_bachillerato <- filter(educacion, id_niveleducativo == 5,
                           sostenimiento_educ == "Publico", ano == 2010) %>%
  dplyr::select(c(1:6,9,15,23))

#Unión de columnas de diferente nivel educativo (Primara, Secundaria, Bachillerato).
edu_psb <-  inner_join(edu_primaria, edu_secundaria, 
                     by = c("ano", "id_municipio", "id_estado", "municipio",
                        "cve_inegi"), suffix = c("_primaria", "")) %>%
          inner_join(edu_bachillerato,
                     by=c("ano", "id_municipio", "id_estado", "municipio",
                           "cve_inegi"), suffix = c("_secundaria","_bachillerato")) %>%
  dplyr::select(-c(estado_primaria, estado_secundaria, estado_bachillerato))

#Selección de columnas importantes en IDH
IDH <- dplyr::select(IDH,-c(7,9))

#Unión de IDH con edu_psb

df_final <- inner_join(edu_psb, IDH, by = c("ano", "id_municipio",
                                               "id_estado", "municipio","cve_inegi"))


#Se van a quitar todas las columnas que no sean datos o la variable dependiente.
#En el Summary se observa que las variables  ieduc y iing tienen 
#miles de NA's cada uno, se van a quitar esas variables.

summary(df_final) 

df_final_importantes <- dplyr::select(df_final, 6:14, 16:21,33)

#Pasamos la variable GANADOR a clase factor para que la función "relevel" funcione

df_final_importantes$GANADOR = as.factor(df_final_importantes$GANADOR)

############################################################
#Regresión para las 3 categorías. Se van a buscar las mejores variables independientes
#Todavía no se tomó en cuenta esto para el modelo, pero ver si sí.
df_final_importantes$GANADOR <- relevel(df_final_importantes$GANADOR, ref = "AMLO")
full.model <- multinom(GANADOR ~ ., df_final_importantes)
summary(full.model)

#Se usa AKAIKE para selecionar las mejores variables independientes.

#Se usa la función stepAIC y se elige trace=TRUE para obtener detalles del 
#proceso de selección. Se aplica el método "backward".

modback <- stepAIC(full.model, trace=TRUE, direction="backward", steps = 2000)

#Para obtener un resumen del proceso se usa

modback$anova

#Ver la tabla de resultados del modelo modback. Observar que se redujo el modelo a las 
#14 variables más significativas.Stargazer crea un html con la tabla en la carpeta contenedora.

summary(modback)  
stargazer(modback, type="html",
          out="Tabla.html") #stargazer va a generar la tabla en la carpeta donde se tenga el archivo de R.
confint(modback, level = 0.95 )

#Se hace una nueva tabla con las variables más significativas
#Ver si egresar si es que se tienen más variables
##################################################################3


#Modelos de clasificación

#Acá hay que meter sólo de los municipios que el profesor dio. 
#Creé una lista del 85% de las filas en el conjunto de datos original 
#para usar en el entrenamiento por lo mientras

validation_index <- createDataPartition(df_final_importantes$GANADOR, p=0.85, list = FALSE)

# seleccionar el 15% de los datos para la validación

validation <- df_final_importantes[-validation_index,]

# Se usa el 85% de los datos para entrenar y probar los modelos

train_test <- df_final_importantes[validation_index,]

# Ejecutar algoritmos usando validación cruzada de 10 veces

control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

#Logistic Regression
set.seed(6)
fit.brnn <- train(GANADOR~., train_test, method="multinom", metric = metric, trControl = control)

#Rainforest model (#Se probó este y otros para ver si se conseguía más precisión sacrificando interpretación, pero no aumentó.)
set.seed(6)
fit.rf <- train(GANADOR~.,  train_test, method="rf", metric=metric, trControl=control)

# resumen de la precisión de los modelos 
results <- resamples(list(multinom = fit.brnn, rf=fit.rf))
summary(results)

# comparar la precisión de los modelos
dotplot(results)

# resumen del mejor modelo: logistic regression
print(fit.brnn)

#estimar la habilidad en el conjunto de datos de validación

predictions <- predict(fit.brnn, validation)
confusionMatrix(predictions, validation$GANADOR)



