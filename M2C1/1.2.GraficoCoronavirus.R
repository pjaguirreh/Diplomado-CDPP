######################
## Cargar librerías ##
######################
library(readr) # Cargar datos
library(ggplot2) # Visualizar datos
library(dplyr) # Manejo de datos

## Cargar datos
# Datos creados a partir de API generada por *@pachamaltese* (https://github.com/pachamaltese) para DATA UC (https://coronavirus.mat.uc.cl/)
# Si alguien quiere profundizar en como se crea el archivo "covid_datos_region.csv" el script "0.GenerarBBDDCoronavirus.R" lo describe
datos_covid <- read_csv("datos/covid_datos_region.csv")
datos_covid

# Examinar datos
str(datos_covid)
summary(datos_covid)

######################
## Visualizar datos ##
######################

# Gráfico base
ggplot(datos_covid, aes(x = fecha, y = casos_covid_pob))

# Agregar `geom` de puntos
ggplot(datos_covid, aes(x = fecha, y = casos_covid_pob)) +
  geom_point()

# Podemos también cambiar el color, forma, y tamaño de los puntos
ggplot(datos_covid, aes(x = fecha, y = casos_covid_pob)) +
  geom_point(col = "red", size = 2, shape = 3)

# Podría ser también un `geom` de lineas
# Pero algo se ve mal
ggplot(datos_covid, aes(x = fecha, y = casos_covid_pob)) +
  geom_line()

# Hay que darle más información/instrucciones al gráfico

# Cada linea representa una región
ggplot(datos_covid, aes(x = fecha, y = casos_covid_pob, group = region)) +
  geom_line()

# Cada linea representa una región
ggplot(datos_covid, aes(x = fecha, y = casos_covid_pob, col = region)) +
  geom_line()

# Lo que se defina dentro de `aes` en `ggplot()` afectará todos los `geom`
ggplot(datos_covid, aes(x = fecha, y = casos_covid_pob, col = region)) +
  geom_line() +
  geom_point()

# Si no queremos que esto pase, podemos definir `aes` para `geom` especifícos
ggplot(datos_covid, aes(x = fecha, y = casos_covid_pob)) +
  geom_line(aes(col = region)) +
  geom_point(size = 0.5)

######

# Separemos cada linea en su propio panel 

ggplot(datos_covid, aes(x = fecha, y = casos_covid_pob)) +
  geom_line(aes(col = region), size = 1) +
  facet_wrap(vars(region ))

# Separemos cada linea en su propio panel 

ggplot(datos_covid, aes(x = fecha, y = casos_covid_pob)) +
  geom_line(aes(col = region), size = 1) +
  facet_wrap(vars(region), scales = "free_y")

# Separemos cada linea en su propio panel 

ggplot(datos_covid, aes(x = fecha, y = casos_covid_pob)) +
  geom_line(aes(col = region), size = 1) +
  facet_wrap(vars(region), ncol = 5)

# Fondo blanco pareciera quedar mejor

ggplot(datos_covid, aes(x = fecha, y = casos_covid_pob)) +
  geom_line(aes(col = region), size = 1) +
  facet_wrap(vars(region)) +
  theme_bw()

# Fondo blanco pareciera quedar mejor

ggplot(datos_covid, aes(x = fecha, y = casos_covid_pob)) +
  geom_line(aes(col = region), size = 1) +
  facet_wrap(vars(region)) +
  theme_minimal()

# La leyenda pareciera no ser de mucha ayuda

ggplot(datos_covid, aes(x = fecha, y = casos_covid_pob)) +
  geom_line(aes(col = region), size = 1) +
  facet_wrap(vars(region)) +
  theme_minimal() +
  theme(legend.position = "none")

# Títulos/Ejes como detalles finales

ggplot(datos_covid, aes(x = fecha, y = casos_covid_pob)) +
  geom_line(aes(col = region), size = 1) +
  facet_wrap(vars(region)) + theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Evolución casos COVID-19 en Chile por región",
       subtitle = "La región de Tarapacá y Metropolitana lideran la tabla",
       x = NULL, y = "Casos por cada 10.000 habitantes")

## Otros tipos de gráficos
library(dplyr) # Manejo de datos
covid_ultimodia <- datos_covid %>% 
  group_by(region) %>% 
  filter(row_number() == n())

# Gráfico de barras (i)

ggplot(covid_ultimodia, aes(x = region, y = casos_covid_pob)) + 
  geom_col()

# Gráfico de barras (ii)

ggplot(covid_ultimodia, aes(x = reorder(region, casos_covid_pob), y = casos_covid_pob)) + 
  geom_col()

# Gráfico de barras (iii)

ggplot(covid_ultimodia, aes(x = reorder(region, casos_covid_pob), y = casos_covid_pob)) + 
  geom_col(fill = "light blue") +
  theme_minimal() + 
  labs(x = NULL, y = "Casos COVID") +
  coord_flip()

# Boxplot (i)

ggplot(datos_covid, aes(region, casos_covid)) +
  geom_boxplot()

# Boxplot (ii)

datos_covid %>% 
  ggplot(aes(region, log(casos_covid))) +
  geom_boxplot() + 
  theme_minimal()

# Boxplot (iii)

ggplot(datos_covid, aes(region, log(casos_covid))) +
  geom_boxplot(fill = "light blue") + 
  theme_minimal() + 
  labs(x = NULL, y = "Log de Casos COVID") +
  coord_flip()

# Dispersión y linea de tendencia (i) 

ggplot(covid_ultimodia, aes(pcr_acum_pob, log(muertes_covid))) +
  geom_point(size = 3, col = "light blue")

# Dispersión y linea de tendencia (ii) 

ggplot(covid_ultimodia, aes(pcr_acum_pob, log(muertes_covid))) +
  geom_point(size = 3, col = "light blue") + 
  geom_smooth() +
  theme_minimal() + 
  labs(x = "Casos COVID", y = "Log de muertes por COVID")

# Dispersión y linea de tendencia (iii) 

ggplot(covid_ultimodia, aes(pcr_acum_pob, log(muertes_covid))) +
  geom_point(size = 3, col= "light blue") + 
  geom_smooth(method = "lm", se = FALSE, col = "black") +
  theme_minimal() + 
  labs(x = "Casos COVID", y = "Log de muertes por COVID")