######################
## Cargar librerías ##
######################
library(readr) # Cargar datos
library(ggplot2) # Visualizar datos

##################
## Cargar datos ##
##################
datos_covid <- read_csv("../datos/DatosCOVIDRegion.csv") %>% 
  filter(region != "Total")

################
## Ejercicios ##
################

## Donde vea "***" es donde debe escribir algo

# Guarde en el objeto "p" un gráfico de lineas con las variables fecha en el eje X y fallecidos en el eje Y.
# Distinga cada linea por región con un color distinto y que todas tengan tamaño 1.
(p <- ggplot(datos_covid, aes(x = fecha, y = fallecidos)) + 
  geom_line(aes(col = region), size = 1))

# Separe cada linea en su propio subgrafico (facet por región), dejando la distribución de todos los gráficos
# en 3 columnas. Aplique también un "theme" para dejar fondo de todos los gráficos en blanco y elimine la leyenda
(p <- p +
  facet_wrap(vars(region), ncol= 3, scales = "free_y") + 
  theme_minimal() + 
  theme(legend.position = "none"))

# Agregue títulos y subtítulos al gráfico y modifique texto de ejes X e Y.
p +
  labs(title = "Lo que quiera",
       subtitle = "Lo que quiera",
       x = "Lo que quiera", 
       y = "Lo que quiera")
