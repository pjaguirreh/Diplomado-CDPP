######################
## Cargar librerías ##
######################
library(ggplot2) # Visualizar datos

##################
## Cargar datos ##
##################
iris

################
## Ejercicios ##
################

## Donde vea "***" es donde debe escribir algo

***(iris, ***(x = Sepal.Width, y = Sepal.Length, *** = Species)) + 
  geom_***() *** 
  geom_***(se = ***) ***
  labs(
    *** = "Relación entre ancho y largo de sépalo",
    *** = "Ancho de sépalo (mm)",
    *** = "Largo de sépalo (mm)"
  ) ***
  ***_minimal()