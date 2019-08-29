library(openxlsx)
library(tidyverse)

setwd("C:\\Users\\pjagu\\Dropbox\\Diplomado Big data\\Versión 2019\\datos_originales")
datos <- read.xlsx("datos_obesidad_gdp.xlsx")

names(datos)

a <- filter(datos, Region == "Latin America & Caribbean")
b <- datos[datos$Region == "Latin America & Caribbean" & !is.na(datos$Region),]

a <- select(datos, 2, 4)
b <- datos[,c(2,4)]

