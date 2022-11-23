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
IDIM <- read_csv("IDIM_Nacional.csv") %>%
  as.data.frame()

#Estos son los municipios que dio el profesor
mun_entrenamiento <- read_csv("PRE_2018_MUNICIPIOS.csv") %>% 
  as.data.frame()

#Se buscan NA's en los dataframes. 

sum(is.na(eco))
sum(is.na(educacion))
sum(is.na(IDH))# hay 9836 NA's, summary para ver qué quitar
summary(IDH)
sum(is.na(IDIM))#la columna que tiene la NA no la vamos a ocupar

#Se va a quitar la columna ieduc e iing pues tienen miles de NA´s, los otros
#NA's son muy pocos, sólo 2 en algunas variables. Para no perder los datos
#se va a sustituir por el promedio de cada variable.

#Eliminación de ieduc e iing

IDH <- dplyr::select(IDH, -c(ieduc,iing))

#Sustitución de NA's por su promedio:
#creo que se puede hacer esto sin tantos renglones. 

IDH$idh[is.na(IDH$idh)]<- mean(IDH$idh,na.rm = TRUE)
IDH$tmi[is.na(IDH$tmi)]<- mean(IDH$tmi,na.rm = TRUE)
IDH$talfa[is.na(IDH$talfa)]<- mean(IDH$talfa,na.rm = TRUE)
IDH$tae[is.na(IDH$tae)]<- mean(IDH$tae,na.rm = TRUE)
IDH$ipca[is.na(IDH$ipca)]<- mean(IDH$ipca,na.rm = TRUE)
IDH$isal[is.na(IDH$isal)]<- mean(IDH$isal,na.rm = TRUE)



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

#Unión de IDH con edu_psb. 

edupsb_IDH <- inner_join(edu_psb, IDH, by = c("ano", "id_municipio",
                                               "id_estado", "municipio",
                                              "cve_inegi"))

#Creación de data frame con variables seleccionadas. Con este dataframe, "all_data",
#se va a correr el AKAIKE para conseguir las variables que realmente son las más
#importantes. 

all_data <- inner_join(edupsb_IDH, IDIM, 
                       by = c("cve_inegi" = "Clave del Municipio")) %>%
  dplyr::select(-c(1,2,15,16,18,24:28,36:41))

#El akaike se realizó en el otro script, ahora se hace el
#dataframe con las variables más importantes que arrojó el AKAIKE

importantes_conClaves <- dplyr::select(all_data,-c(8,16,20,21,24,25)) 


#se seleccionan las columnas con ID y GANADOR de los datos del profesor,
#es decir, los de entrenamiento

mun_entrenamiento <- dplyr::select(mun_entrenamiento, -c(2,4:11))

#Se hace match de nuestros datos con los datos de entrenamiento y luego
#se eliminan las columnas de id.

entrenamiento <- inner_join(importantes_conClaves, mun_entrenamiento,
                            by = c("id_estado" = "ID_ESTADO",
                                   "id_municipio" = "ID_MUNICIPIO")) %>%
  dplyr::select(-c(1,2,3))

#Modelos de clasificación

#Se creó una lista del 85% de las filas en el conjunto de datos original 
#para usar en el entrenamiento por lo mientras.


validation_index <- createDataPartition(entrenamiento$GANADOR, p=0.85, list = FALSE)

# seleccionar el 15% de los datos para la validación

validation <- entrenamiento[-validation_index,]

# Se usa el 85% de los datos para entrenar y probar los modelos

train_test <- entrenamiento[validation_index,]

# Ejecutar algoritmos usando validación cruzada de 10 veces

control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

#Multinomial regression
set.seed(6)
fit.multinom <- train(GANADOR~., train_test, method="multinom", metric = metric, trControl = control)

#Linear discriminant analysis
set.seed(6)
fit.lda <- train(GANADOR~.,  train_test, method="lda", metric=metric, trControl=control)

# resumen de la precisión de los modelos 
results <- resamples(list(multinom = fit.multinom, lda=fit.lda))
summary(results)

# comparar la precisión de los modelos
dotplot(results)

# resumen del mejor modelo: logistic regression
print(fit.multinom)

#estimar la habilidad con el conjunto de datos de validación

predictions <- predict(fit.multinom, validation)
confusionMatrix(predictions, as.factor(validation$GANADOR))

#Acá se compara e valor real com la predicción, pero no sale la id... estoy intentando eso
actual_vs_predicted <- data.frame(actual= validation$GANADOR, predicted=predictions) 
View(actual_vs_predicted)


