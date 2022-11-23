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

#Creación de data frame con variables seleccionadas. Con este dataframe, "all_data_akaike",
#se va a correr el AKAIKE para conseguir las variables que realmente son las más
#importantes. 

all_data <- inner_join(edupsb_IDH, IDIM, 
                       by = c("cve_inegi" = "Clave del Municipio"))
all_data_akaike <- dplyr::select(all_data,-c(1:5,15,16,18,24:28,36:41))

#El akaike se realizó en el otro script, ahora se hace el
#dataframe con las variables más importantes que arrojó el AKAIKE

importantes_conClaves <- dplyr::select(all_data,-c(1, 10,15,16, 18,24:28
                                                   ,30,31,34:41)) 

#dataframe con municipio,id, número de votos y ganador
importantes_conClaves_entrenamiento <- inner_join(importantes_conClaves, mun_entrenamiento,
                                    by = c("id_municipio" = "ID_MUNICIPIO",
                                           "id_estado" = "ID_ESTADO")) 

#dataframe con municipio, id y ganador
importantes_conClaves_ganador <- dplyr::select(importantes_conClaves_entrenamiento, -c(22:30))


#Sólo se renombra la variable
entrenamiento <- importantes_conClaves_ganador

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
fit.QdaCov <- train(GANADOR~alumnos_total_primaria + docentes_total_primaria +
                    escuelas_primaria + alumnos_total_bachillerato +
                      docentes_total_bachillerato + escuelas_bachillerato +
                      idh + tmi + talfa + ipca + isal + indice_de_rezago_social +
                      + prestación_de_servicios_públicos +
                      desarrollo_administrativo, 
                    train_test, method="QdaCov", metric = metric, trControl = control)

#Linear discriminant analysis
set.seed(6)
fit.lda <- train(GANADOR~alumnos_total_primaria + docentes_total_primaria +
                   escuelas_primaria + alumnos_total_bachillerato +
                   docentes_total_bachillerato + escuelas_bachillerato +
                   idh + tmi + talfa + ipca + isal + indice_de_rezago_social +
                   + prestación_de_servicios_públicos +
                   desarrollo_administrativo,  train_test, method="lda", metric=metric, trControl=control)
#Rain Forest
set.seed(6)
fit.rf <- train(GANADOR~alumnos_total_primaria + docentes_total_primaria +
                   escuelas_primaria + alumnos_total_bachillerato +
                   docentes_total_bachillerato + escuelas_bachillerato +
                   idh + tmi + talfa + ipca + isal + indice_de_rezago_social +
                   + prestación_de_servicios_públicos +
                   desarrollo_administrativo,  train_test, method="rf", metric=metric, trControl=control)



# resumen de la precisión de los modelos 
results <- resamples(list(QdaCov = fit.QdaCov, lda=fit.lda, rf = fit.rf))
summary(results)

# comparar la precisión de los modelos
dotplot(results)

# resumen del mejor modelo: multinomial regression
print(fit.QdaCov)

#estimar la habilidad con el conjunto de datos de validación

predictions <- predict(fit.QdaCov, validation)
confusionMatrix(predictions, as.factor(validation$GANADOR))

#Acá se compara e valor real com la predicción, pero no sale la id... estoy intentando eso
actual_vs_predicted <- data.frame(actual = validation, predicted=predictions) %>%
  dplyr::select(c(1:4,22,23))


View(actual_vs_predicted)


