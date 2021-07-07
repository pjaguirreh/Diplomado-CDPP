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
ggplot(datos_alcaldes, aes(x = ***, 
                           y = ***)) +
  geom_col() +
  coord_***()

# Repita todo lo recién hecho, sume un titulo y subtitulo, y elimine los títulos de
# los ejes X e Y
ggplot(datos_alcaldes, aes(x = ***, 
                           y = ***)) +
  geom_col() +
  coord_***() +
  labs(*** = "Elecciones de alcaldes 2021",
       *** = "Votos por partido político",
       *** = NULL, y = ***) +
  theme_minimal()


# Repita todo lo recién hecho pero agregue etiquetas a cada barra correspondientes
# al n° de votos
ggplot(datos_alcaldes, aes(x = ***, 
                           y = ***,
                           label = ***)) +
  geom_col() +
  geom_text() +
  coord_***() +
  labs(*** = "Elecciones de alcaldes 2021",
       *** = "Votos por partido político",
       *** = NULL, y = ***) +
  theme_minimal()


# Repita lo recién hecho y vea como los distintos argumentos agregados por defecto
# cambian la apariencia del gráfico
ggplot(datos_alcaldes, aes(x = reorder(***, votos), 
             y = ***,
             label = prettyNum(***, big.mark = "."))) +
  geom_col() +
  geom_text(size = 2.2, 
            nudge_y = 56000,
            nudge_x = 0.15) +
  coord_***() +
  labs(*** = "Elecciones de alcaldes 2021",
       *** = "Votos por partido político",
       *** = NULL, y = ***) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 7))
