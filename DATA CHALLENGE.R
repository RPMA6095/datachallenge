install.packages("dplyr")
library(readr)
library(dplyr)

ECO <- read_csv("ECO.csv")
EDUCACION <- read_csv("EDUCACION.csv")
IDH <- read_csv("IDH.csv")
MUNICIPIOS2018 <- read_csv("PRE_2018_MUNICIPIOS.csv")
a=merge(EDUCACION, ECO, by=c("ano", "id_municipio", "id_estado","municipio", "estado"))
View(a)
df=merge(a, IDH, by=c("ano", "id_municipio", "id_estado","municipio", "estado"))
colnames(df)=toupper(colnames(df))
base=MUNICIPIOS2018%>% select (-MUNICIPIO)
df=merge(x = base, y = df, all.x = TRUE)


#MODELO LINEAL
