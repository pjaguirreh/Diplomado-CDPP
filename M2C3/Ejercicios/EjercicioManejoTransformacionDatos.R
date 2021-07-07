######################
## Cargar librerías ##
######################
library(readr) # Cargar datos
library(dplyr) # Manejo de datos
library(tidyr) # Transformación de datos
library(stringr) # Manejo de datos tipo texto
library(ggplot2) # Visualizar datos

##################
## Cargar datos ##
##################
datosONU <- read_csv("../datos/DatosONU_select.csv") %>% 
  select(-X1, -`Series Code`) 

################
## Ejercicios ##
################

## Donde vea "***" es donde debe escribir algo

# Modifique la forma de los datos de "ancho" a "largo". Tome las primeras 36 columnas y asigne los nombres a una 
# nueva variable "anio" y sus valores correspondientes a una columna "valor".
(datosONU2 <- datosONU %>% 
  pivot_***(1:36, ***_to = "anio", ***_to = "valor") )

# Cambie el nombre de las columnas "Country Name" y "Series Name" a "pais" e "indicador", respectivamente
(datosONU3 <- datosONU2 %>% 
  ***(
    *** = `Country Name`,
    *** = `Series Name`))

# Cargar datos complementarios
region <- read_csv("datos/region.csv")
grupo_ingresos <- read_csv("datos/income_group.csv")

# Una "datosONU3" a las bases "region" y "grupo_ingresos". Asegurese de ver que columnas tienen en común.
# Cambie el nombre de "income group" a "grupo_ingresos".
# Ordene la base para que queden las columnas en el siguiente orden: pais, region, grupo_ingresos, y el resto.
(datosONU4 <- datosONU3 %>% 
  left_***(***, by = c("pais" = "country_name")) %>% 
  left_***(grupo_ingresos, *** = c("pais" = "country_name")) %>% 
  ***(grupo_ingresos = income_group) %>% 
  select(***, ***, ***, everything()))

# Cambie el nombre del valor de la columna "indicador" correspondiente a "CO2 emissions (metric tons per capita)" 
# a una forma más simple en español.
(datosONU5 <- datosONU4 %>% 
  ***(
    indicador = case_***(
      indicador == *** ~ "emisiones_co2_percap",
      TRUE ~ indicador)
    ))

# Cambie los nombres de las variables "grupo_ingresos" y "region"  español. 
# En el caso de la variable "grupo ingresos", fusione Lower y Upper Middle Income en una sola categoria.
(datosONU6 <- datosONU5 %>%
  ***(
    grupo_ingresos = ***(
      grupo_ingresos == "Low Income" ~ "Ingresos Bajos",
      grupo_ingresos *** c("Lower Middle Income", "Upper Middle Income") ~ "Ingresos Medios",
      grupo_ingresos == "High Income" ~ "Ingresos Altos"),
    region = ***(
      region == "East Asia and Pacific" ~ "Asia Oriente y Pacifico",
      region == "Europe and Central Afica" ~ "Europa y Africa Central",
      region == "Latin America and the Caribbean" ~ "Latinoamerica y el Caribe",
      region == "Middle East and North Africa" ~ "Medio Oriente y Africa del Norte",
      region == "North America" ~ "Norte America",
      region == "South Asia" ~ "Asia del sur",
      region == "Sub-saharan Africa" ~ "Africa subsahariana")))

# Asigne los valores de "indicador" como columnas y complete los valores con la columna "valor"
(datosONU7 <- datosONU6 %>% 
  pivot_***(***_from = indicador, ***_from = valor))

# Sobreescriba la columna "anio" extrayendo solo el valor numérico correspondiente. Asegurese que
# la variable quede como tupo numérico y no texto.
(datosONU8 <- datosONU7 %>% 
  ***(anio = str_sub(***, 1, 4),
      anio = as.numeric(***)))

# Tomando solo datos del año 2007, calcule el promedio de "emisiones_co2_percap" para cada combinación de
# grupo_ingresos y region
(datosONU9 <- datosONU8 %>% 
  filter(anio == ***) %>% 
  ***(grupo_ingresos, region) %>% 
  summarise(emisiones_co2_percap = ***(emisiones_co2_percap, na.rm = TRUE)))

# Genere un gráfico de barras con region como eje X, emisiones_co2_percap com eje Y
# y que el relleno de cada barra diferencie entre grupo_ingresos.
datosONU9 %>% 
  ***(aes(x = ***, y = ***, fill = grupo_ingresos)) +
  ***_col() +
  coord_flip()

# Genere una tabla con regiones como filas y grupos de ingreso como columnas.
datosONU9 %>% 
  pivot_***(***_from = ***, ***_from = ***,
            values_fill = 0)
