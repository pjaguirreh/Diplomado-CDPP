######################
## Cargar librerías ##
######################
library(readxl) # Cargar datos Excel
library(ggplot2) # Visualizar datos

##################
## Cargar datos ##
##################
datos_alcaldes <- read_excel("../datos/alcaldes_2021_ResumenPartidos.xlsx")

# Explorar datos
summary(datos_alcaldes)

################
## Ejercicios ##
################

## Donde vea "***" es donde debe escribir algo

# Haga un gráfico de barras con el nombre de los partidos en el eje X 
# y los números de votos en el eje Y. Finalmente, de vuelta los ejes X e Y
ggplot(datos_alcaldes, aes(x = partido_nom, 
                           y = votos)) +
  geom_col() +
  coord_flip()

# Repita todo lo recién hecho, sume un titulo y subtitulo, y elimine los títulos de
# los ejes X e Y
ggplot(datos_alcaldes, aes(x = partido_nom, 
                           y = votos)) +
  geom_col() +
  coord_flip() +
  labs(title = "Elecciones de alcaldes 2021",
       subtitle = "Votos por partido político",
       x = NULL, y = NULL) +
  theme_minimal()


# Repita todo lo recién hecho pero agregue etiquetas a cada barra correspondientes
# al n° de votos
ggplot(datos_alcaldes, aes(x = partido_nom, 
                           y = votos,
                           label = votos)) +
  geom_col() +
  geom_text() +
  coord_flip() +
  labs(title = "Elecciones de alcaldes 2021",
       subtitle = "Votos por partido político",
       x = NULL, y = NULL) +
  theme_minimal()


# Repita lo recién hecho y vea como los distintos argumentos agregados por defecto
# cambian la apariencia del gráfico
ggplot(datos_alcaldes, aes(x = reorder(partido_nom, votos), 
             y = votos,
             label = prettyNum(votos, big.mark = "."))) +
  geom_col() +
  geom_text(size = 2.2, 
            nudge_y = 56000,
            nudge_x = 0.15) +
  coord_flip() +
  labs(title = "Elecciones de alcaldes 2021",
       subtitle = "Votos por partido político",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 15))
