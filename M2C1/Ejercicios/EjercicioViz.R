# Cargar paquetes
library(readr)
library(ggplot2)

#setwd("...")

# Cargar datos
datos_ejercicio <- read_csv("../datos/datos_mundo_ejercicio.csv")

###############
## EJERCICIO ##
###############

# Complete los espacios con *** según corresponda

# Hacer un histograma de "ExpVida" utilizando 15 divisiones/barras (bins)
ggplot(***, aes(x = ***)) +
  geom_histogram(bins = ***)

# Hacer un gráfico de puntos de "anio" (x) vs "ExpVida" (y)
ggplot(datos_ejercicio, aes(x = ***, y = ***)) +
  geom_***()

# Repetir el gráfico anterior y diferenciar los puntos con un color distinto según su continente
ggplot(datos_ejercicio, aes(x = ***, y = ***, *** = continente)) +
  geom_***()

# Agregar al gráfico anterior un geom de lineas (geom_line)
ggplot(datos_ejercicio, ***(x = ***, y = ***, *** = continente)) +
  geom_***() +
  geom_***()

# Repetir el gráfico anterior para las otras dos variables presentes en "datos_ejercicio"

# Modifique los valores de col; size; shape; alpha; linetype
# Y analice lo que ocurre
ggplot(datos_ejercicio, aes(x = anio, y = ExpVida, group = continente)) +
  geom_point(col = 1, 
             shape = 16, 
             size = 1.5, 
             alpha = 0.9) + # alpha entre 0 y 1
  geom_line(linetype = 1,
            size = 0.5)
