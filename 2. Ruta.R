########
##Ruta##
########

rm(list = ls())

#Cargar "prueba.RData"
load("prueba.RData") # R no encuentra el archivo

#Obtener la carpeta (directorio) que R ocupa por defecto
getwd()

#Definir otra carpeta
setwd("C:/Users/pjagu/Dropbox/Diplomado Big data/Versión 2019/datos_originales/") #"C:\\Users\\pjagu\\Dropbox\\Diplomado Big data\\Versión 2019\\datos_originales"
getwd() #ver como cambia el "working directory

load("prueba.RData") # Ahora no hay problema para cargar el archivo

rm(list = ls()) # Eliminamos todos los objetos del "Global Environment"

setwd("..") # Busca define como "working directory" un nivel superior al que estamos

setwd("datos_originales") # Forma m?s sintética de definir el working directory

load("/prueba.RData")


