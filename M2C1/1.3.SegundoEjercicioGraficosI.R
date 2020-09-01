######################
## Cargar librerías ##
######################
library(readr) # Cargar datos
library(ggplot2) # Visualizar datos

##################
## Cargar datos ##
##################
datos_covid <- read_csv("datos/covid_datos_region.csv")

################
## Ejercicios ##
################

## Donde vea "***" es donde debe escribir algo

# Creen un gráfico de puntos (geom_point) con "fecha" en el eje X y el número de PCR cada 10.000 habitantes (pcr_acum_pob) 
# en el eje Y
ggplot(datos_covid, aes(x = ***, y = ***)) +
  geom_***()
  
# Creen un gráfico de lineas (geom_line) con "fecha" en el eje X y el número de PCR cada 10.000 habitantes (pcr_acum_pob) 
# en el eje Y. Agregue un argumento de color (col) que permita diferenciar las lineas según region
ggplot(datos_covid, aes(x = ***, y = ***, *** = region)) +
  geom_***() 

# Agregue al gráfico anterior puntos (geom_point) y deje el tamaño (size) igual a 1.
ggplot(datos_covid, aes(x = fecha, y = pcr_acum_pob, col = region)) +
  geom_line() +
  geom_***(*** = ***)

# Investigue que ocurre al cambiar los valores de:
# col; size; shape; alpha; linetype
# Intercambie la posicion de las lineas de geom_line y geom_point. ¿pasa algo?
ggplot(datos_covid, aes(x = fecha, y = pcr_acum_pob, col = region)) +
  geom_line(linetype = 1) +
  geom_point(col = "red", size = 3, shape = 16, alpha = 0.5)
