######################
## Cargar librerías ##
######################
library(readr) # Cargar datos
library(ggplot2) # Visualizar datos
library(dplyr) # Manejo de datos

##################
## Cargar datos ##
##################
datos_covid <- read_csv("datos/covid_datos_region.csv")
covid_ultimodia <- datos_covid %>% 
  group_by(region) %>% 
  filter(row_number() == n())

################
## Ejercicios ##
################

## Donde vea "***" es donde debe escribir algo

# Cree un gráfico de barras region vs pcr_acum_pob. Asegúrese de voltear las coordenadas del gráfico y defina
# el relleno de las barras como rojo ("red").
ggplot(covid_ultimodia, aes(x = region, y = pcr_acum_pob)) + 
  geom_col(*** = "red") +
  theme_minimal() + 
  labs(x = NULL, y = "N° PCR acumulados") +
  ***_flip()

# Repita lo recién hecho pero además ajuste el orden en que se muestran las barras
ggplot(covid_ultimodia, aes(x = ***(region, pcr_acum_pob), y = pcr_acum_pob)) + 
  geom_col(*** = "red") +
  theme_minimal() + 
  labs(x = NULL, y = "N° PCR acumulados") +
  ***_flip()

# Cree un gráfico boxplot y defina el relleno como café ("brown"). Ajuste también la transparencia (alpha) de los
# boxplot a 0.5
datos_covid %>% 
  ggplot(aes(region, log(casos_covid))) +
  geom_boxplot(fill = ***, *** = 0.5) + 
  theme_minimal() + 
  labs(x = NULL, y = "Log de Casos COVID") +
  coord_flip()
