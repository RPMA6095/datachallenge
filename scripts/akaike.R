#Esta parte es  para selección de variables.
######NO ES NECESARIO CORRERLO#####
#######################################################

#Para seleccionar las más importantes se va a correr un AKAIKE uniendo los
#datos que se tienen con los datos del municipio que dio el profesor. 
#Luego se va a crear un data frame con las variables más representativas. 
#Estas se van a utiilizar para construir el modelo de clasifiación.

akaike <- inner_join(all_data, mun_entrenamiento, by = 
                       c("id_municipio" = "ID_MUNICIPIO",
                         "id_estado" = "ID_ESTADO")) %>%
  dplyr::select(-c(1:5,15,16,18,24:28,30,36:50))

#Pasamos la variable GANADOR a clase factor para que la función "relevel" funcione

akaike$GANADOR = as.factor(akaike$GANADOR)


#Regresión para las 3 categorías. Se van a buscar las mejores variables independientes

akaike$GANADOR <- relevel(akaike$GANADOR, ref = "AMLO")


full.model <- multinom(GANADOR ~ ., akaike)
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


#Se hace una nueva tabla con las variables más significativas
#Ver si egresar si es que se tienen más variables