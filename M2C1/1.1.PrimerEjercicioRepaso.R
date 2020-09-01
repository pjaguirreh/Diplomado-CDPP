#1) Cargar paquetes
library(readr) # para cargar datos
library(dplyr) # para manejo de datos
library(ggplot2) # para visualizar

#2) Cargar datos
datos_mundo <- read_csv("datos/datos_mundo.csv")
datos_mundo

#3) Filtrar datos
datos_mundo_select <- datos_mundo %>% 
  filter(anio == 2007) # solo deja observaciones donde anio es igual a 2007

#4) Generar grafico
ggplot(datos_mundo_select, aes(x = ExpVida, y = pob)) +
  geom_point()

# EJERCICIO #
# Crear un grafico de gdpPercap (x) vs ExpVida (y) considerando solo datos del 2002
## Donde vea "***" es donde debe escribir algo

# Crear subset con datos 2002
datos_mundo_select <- datos_mundo %>% 
  filter(anio == ***)

# Generar gráfico gdpPercap (x) vs ExpVida (y)
ggplot(datos_mundo_select, aes(x = ***, y = ***)) +
  geom_point()
