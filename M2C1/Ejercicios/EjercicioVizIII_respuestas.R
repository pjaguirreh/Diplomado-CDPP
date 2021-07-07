######################
## Cargar librerías ##
######################
library(readr) # Cargar datos
library(ggplot2) # Visualizar datos
library(dplyr) # Manejo de datos

##################
## Cargar datos ##
##################
datos_covid <- read_csv("../datos/DatosCOVIDRegion.csv") %>% 
  filter(region != "Total")

covid_ultimodia <- datos_covid %>% 
  filter(!is.na(casos_nuevos)) %>% 
  group_by(region) %>% 
  filter(row_number() == n())

################
## Ejercicios ##
################

## Donde vea "***" es donde debe escribir algo

# Cree un gráfico de barras region vs casos_nuevos. Asegúrese de voltear las coordenadas del gráfico y defina
# el relleno de las barras como rojo ("red").
ggplot(covid_ultimodia, aes(x = region, y = casos_nuevos)) + 
  geom_col(fill = "red") +
  theme_minimal() + 
  labs(x = NULL, y = "Casos nuevos") +
  coord_flip()

# Repita lo recién hecho pero además ajuste el orden en que se muestran las barras
ggplot(covid_ultimodia, aes(x = reorder(region, casos_nuevos), y = casos_nuevos)) + 
  geom_col(fill = "red") +
  theme_minimal() + 
  labs(x = NULL, y = "Casos nuevos") +
  coord_flip()

# Cree un gráfico boxplot y defina el relleno como café ("brown"). Ajuste también la transparencia (alpha) de los
# boxplot a 0.5
datos_covid %>% 
  ggplot(aes(region, casos_nuevos)) +
  geom_boxplot(fill = "brown", alpha = 0.5) + 
  theme_minimal() + 
  labs(x = NULL, y = "Log de Casos COVID") +
  coord_flip()

