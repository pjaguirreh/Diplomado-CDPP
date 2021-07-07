# Cargar paquetes y datos
library(readr)
library(dplyr)
library(ggplot2)
(datos_covid <- read_csv("../datos/DatosCOVIDRegion.csv"))
datos_covid <-  filter(datos_covid, region != "Total") #Esto no está en la PPT

## Gráfico base 
ggplot(datos_covid, aes(x = fecha, y = casos_nuevos))

## Agregar `geom` de lineas | Pero algo se ve mal 
ggplot(datos_covid, aes(x = fecha, y = casos_nuevos)) +
  geom_line()

## Una linea para cada región (i) 
ggplot(datos_covid, aes(x = fecha, y = casos_nuevos, group = region)) +
  geom_line()

## Una linea para cada región (ii) 
ggplot(datos_covid, aes(x = fecha, y = casos_nuevos, col = region)) +
  geom_line()

## Cada linea en su propio panel (i) 
ggplot(datos_covid, aes(x = fecha, y = casos_nuevos, col = region)) +
  geom_line() +
  facet_wrap(vars(region ))

## Cada linea en su propio panel (ii) 
ggplot(datos_covid, aes(x = fecha, y = casos_nuevos, col = region)) +
  geom_line() +
  facet_wrap(vars(region), scales = "free_y")

## Cada linea en su propio panel (iii) 
ggplot(datos_covid, aes(x = fecha, y = casos_nuevos, col = region)) +
  geom_line() +
  facet_wrap(vars(region), scales = "free_y", ncol = 5)

## Fondo blanco parece mejor (i) 
ggplot(datos_covid, aes(x = fecha, y = casos_nuevos, col = region)) +
  geom_line() +
  facet_wrap(vars(region), scales = "free_y") +
  theme_bw()

## Fondo blanco parece mejor (ii) 
ggplot(datos_covid, aes(x = fecha, y = casos_nuevos, col = region)) +
  geom_line() +
  facet_wrap(vars(region), scales = "free_y") +
  theme_minimal()

## La leyenda no sirve mucho
ggplot(datos_covid, aes(x = fecha, y = casos_nuevos, col = region)) +
  geom_line() +
  facet_wrap(vars(region), scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none")

## Cambiemos el "theme"
ggplot(datos_covid, aes(x = fecha, y = casos_nuevos, col = region)) +
  geom_line() +
  facet_wrap(vars(region), scales = "free_y") +
  theme_void() + #<<
  theme(legend.position = "none")

# Titulos/ejes
ggplot(datos_covid, aes(x = fecha, y = casos_nuevos, col = region)) +
  geom_line() +
  facet_wrap(vars(region), scales = "free_y") + 
  theme_void() + theme(legend.position = "none") +
  labs(title = "¿Segunda ola?", #<<
       subtitle = "Casos diarios de COVID-19 en Chile por región", 
       x = NULL, y = NULL)

# Último detalle (i)
ggplot(datos_covid, aes(x = fecha, y = casos_nuevos, col = region)) +
  geom_line(aes(y = casos_nuevos_d)) + 
  geom_line() +
  facet_wrap(vars(region), scales = "free_y") + 
  theme_void() + theme(legend.position = "none") +
  labs(title = "¿Segunda ola?",
       subtitle = "Casos diarios de COVID-19 en Chile por región",
       x = NULL, y = NULL) 

# Último detalle (ii)
ggplot(datos_covid, aes(x = fecha, y = casos_nuevos, col = region)) +
  geom_line(aes(y = casos_nuevos_d), alpha = 0.3) +
  geom_line() +
  facet_wrap(vars(region), scales = "free_y") + 
  theme_void() + theme(legend.position = "none") +
  labs(title = "¿Segunda ola?", 
       subtitle = "Casos diarios de COVID-19 en Chile por región",
       x = NULL, y = NULL) 
