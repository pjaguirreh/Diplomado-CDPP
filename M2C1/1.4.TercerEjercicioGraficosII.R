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

# Guarde en el objeto "p" un gráfico de lineas con las variables fecha en el eje X y pcr_acum_pob en el eje Y.
# Distinga cada linea por región con un color distinto y que todas tengan tamaño 2.
p <- ggplot(***, aes(x = ***, y = ***)) + 
  geom_line(***(*** = region), size = ***)

# Separe cada linea en su propio subgrafico (facet por región), dejando la distribución de todos los gráficos
# en 3 columnas. Aplique también un "theme" para dejar fondo de todos los gráficos en blanco y elimine la leyenda
p <- p + 
  ***_wrap(vars(***), ncol= ***) + 
  ***_minimal() + 
  theme(***.position = "none")

# Agregue títulos y subtítulos al gráfico y modifique texto de ejes X e Y.
p +
  labs(title = ***,
       subtitle = ***,
       x = ***, 
       y = ***)
