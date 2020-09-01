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
datosONU <- read_csv("datos/DatosONU_select.csv") %>% 
  select(-X1, -`Series Code`) 

################
## Ejercicios ##
################

## Donde vea "***" es donde debe escribir algo

# Modifique la forma de los datos de "ancho" a "largo". Tome las primeras 36 columnas y asigne los nombres a una 
# nueva variable "anio" y sus valores correspondientes a una columna "valor".
datosONU2 <- datosONU %>% 
  pivot_***(1:36, ***_to = "anio", ***_to = "valor") 

datosONU2

# Cambie el nombre de las columnas "Country Name" y "Series Name" a "pais" e "indicador", respectivamente
datosONU3 <- datosONU2 %>% 
  ***(
    *** = `Country Name`,
    *** = `Series Name`
  ) 

datosONU3

# Cargar datos complementarios
region <- read_csv("datos/region.csv")
grupo_ingresos <- read_csv("datos/income_group.csv")

# Una "datosONU3" a las bases "region" y "grupo_ingresos". Asegurese de ver que columnas tienen en común
# Ordene la base para que queden las columnas en el siguiente orden: pais, region, grupo_ingresos, y el resto
datosONU4 <- datosONU3 %>% 
  left_***(***, by = c("pais" = "country_name")) %>% 
  left_***(grupo_ingresos, *** = c("pais" = "country_name")) %>% 
  ***(grupo_ingresos = income_group) %>% 
  select(***, ***, ***, everything())

datosONU4

# Cambie el nombre de los valores de la variable "indicador" a una forma más simple en español.
datosONU5 <- datosONU4 %>% 
  ***(
    indicador = ***(
      indicador == "CO2 emissions (metric tons per capita)" ~ "emisiones_co2",
      indicador == "Fertility rate, total (births per woman)" ~ "tasa_fertilidad",
      indicador == "Forest area (% of land area)" ~ "area_bosques",
      indicador == "GDP per capita (constant 2005 US$)" ~ "PIB_percapita",
      indicador == "Health expenditure per capita, PPP (constant 2005 international $)" ~ "gasto_medico_percapita",
      indicador == "Labor force participation rate, female (% of female population ages 15+) (modeled ILO estimate)" ~ "participacion_laboral_femenina",
      indicador == "Life expectancy at birth, total (years)" ~ "expectativa_vida",
      indicador == "Malnutrition prevalence, weight for age (% of children under 5)" ~ "malnutricion",
      indicador == "Population (Total)" ~ "poblacion",
      indicador == "Urban population (% of total)" ~ "poblacion_urbana",
      indicador == "Fossil fuel energy consumption (% of total)" ~ "consumo_combustible_fosil)",
      indicador == "Poverty headcount ratio at $2 a day (PPP) (% of population)" ~ "pobreza",
      indicador == "Public spending on education, total (% of government expenditure)" ~ "gasto_publico_educacion"))

datosONU5

# Cambie los nombres de las variables "grupo_ingresos" y "region"  español. En el caso de la variable "grupo ingresos",
# fusione Lower y Upper Middle Income en una sola categoria.
datosONU6 <- datosONU5 %>%
  ***(
    *** = ***(
      grupo_ingresos == "Low Income" ~ "Ingresos Bajos",
      grupo_ingresos *** c("Lower Middle Income", "Upper Middle Income") ~ "Ingresos Medio-Bajo",
      grupo_ingresos == "High Income" ~ "Ingresos Altos"),
    *** = ***(
      region == "East Asia and Pacific" ~ "Asia Oriente y Pacifico",
      region == "Europe and Central Afica" ~ "Europa y Africa Central",
      region == "Latin America and the Caribbean" ~ "Latinoamerica y el Caribe",
      region == "Middle East and North Africa" ~ "Medio Oriente y Africa del Norte",
      region == "North America" ~ "Norte America",
      region == "South Asia" ~ "Asia del sur",
      region == "Sub-saharan Africa" ~ "Africa subsahariana")) 

datosONU6

# Asigne los valores de "indicador" como columnas y complete los valores con la columna "valor"
datosONU7 <- datosONU6 %>% 
  pivot_***(***_from = indicador, ***_from = valor)

datosONU7

# Sobreescriba la columna "anio" extrayendo solo el valor numérico correspondiente. Asegurese que
# la variable quede como tupo numérico y no texto.
datosONU8 <- datosONU7 %>% 
  ***(anio = str_sub(***, 1, 4),
         anio = as.numeric(***))

datosONU8

# Tomando solo datos del año 2007, calcule el promedio de "emisiones_co2" para cada combinación de
# grupo_ingresos y region
datosONU9 <- datosONU8 %>% 
  filter(anio == ***) %>% 
  ***(grupo_ingresos, region) %>% 
  summarise(emisiones_co2 = ***(emisiones_co2, na.rm = TRUE)) 

datosONU9

# Genere una tabla con regiones como filas y grupos de ingreso como columnas.
datosONU9 %>% 
  pivot_***(***_from = ***, ***_from = ***)

#rm(datosONU2, datosONU3, datosONU4, datosONU5, datosONU6, datosONU7, datosONU8)