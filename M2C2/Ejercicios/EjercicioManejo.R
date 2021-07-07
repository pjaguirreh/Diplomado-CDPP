######################
## Cargar librerías ##
######################
library(readr) # Cargar datos
library(dplyr) # Manejo de datos
library(ggplot2) # Visualizar datos

##################
## Cargar datos ##
##################
datosONU_tidy <- read_csv("../datos/DatosONU_tidy.csv")

################
## Ejercicios ##
################

## Donde vea "***" es donde debe escribir algo

### PARTE I ###

# Observe los valores únicos correspondiente a "income_group"
datosONU_tidy %>% distinct(income_group)

# Genere un nuevo objeto datosONU_tidy_new seleccionando las variables country_name, income_group, year,
# gdp_per_capita_constant_2005_us y life_expectancy_at_birth_total_years
datosONU_tidy_new <- datosONU_tidy %>% 
  select(***, ***, ***, ***, ***, ***) 

# Sobreescriba el objeto datosONU_tidy_new filtrando aquellas observaciones con valores de income_group 
# igual a "Upper Middle Income" o "High Income" y año igual a 2007
datosONU_tidy_new <- datosONU_tidy_new %>% 
  filter(income_group %in% c("***", "***"),
         year *** 2007) 

# Cree una nueva variable correspondiente al logaritmo de gdp_per_capita_constant_2005_us
datosONU_tidy_new <- datosONU_tidy_new %>% 
  mutate(log_gdp_per_capita = ***(gdp_per_capita_constant_2005_us))

# Genere un gráfico de puntos (geom_point) entre log_gdp_per_capita (x) y life_expectancy_at_birth_total_years (y)
# Y permita que los puntos tengan un color distinto según el "income_group" al que pertenezcan
datosONU_tidy_new %>%
  ggplot(aes(x = ***, y = life_expectancy_at_birth_total_years, col = ***)) +
  geom_***()

### PARTE II ###

datosONU_tidy_ej <- datosONU_tidy %>% 
  select(country_name, income_group, region, year, population_total)

# Cambia el nombre de las variables a "pais", "grupo_ingresos", "anio", "total_poblacion". region no lo cambien.
datosONU_tidy_ej <- datosONU_tidy_ej %>% 
  ***(
    "pais" = "***",
    "grupo_ingresos" = "***",
    "anio" = "year",
    "total_poblacion" = "***"
  )

# ¿Cual era la poblacion mundial en 2007? (considerando los países de nuestra base)
datosONU_tidy_ej %>% 
  filter(*** == 2007) %>% 
  ***(poblacion_mundial = sum(total_poblacion, na.rm = TRUE))

# En un objeto datosONU_tidy_2007_reg guarden el resultado de calcular el total de poblacion por región
# asi como el número de países para cada region
datosONU_tidy_2007_reg <- datosONU_tidy_ej %>% 
  filter(*** == 2007) %>% 
  group_by(region) %>% 
  ***(poblacion_regional = ***(total_poblacion, na.rm = TRUE),
      n_paises = n())

# Tomando los números recién calculados, creen una nueva columna calculando la población promedio 
# por país para cada región
datosONU_tidy_2007_reg %>% 
  ***(promedio_pob = ***/n_paises)