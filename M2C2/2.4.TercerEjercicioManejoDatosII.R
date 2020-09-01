######################
## Cargar librerías ##
######################
library(readr) # Cargar datos
library(dplyr) # Manejo de datos

##################
## Cargar datos ##
##################
datosONU_tidy <- read_csv("datos/DatosONU_tidy.csv")
datosONU_tidy_ej <- datosONU_tidy %>% 
  select(country_name, income_group, region, year, population_total)

################
## Ejercicios ##
################

## Donde vea "***" es donde debe escribir algo

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
  ***(poblacion_mundial = ***(total_poblacion, na.rm = TRUE),
            n_paises = n())

# Tomando los números recién calculados, creen una nueva columna calculando la población promedio 
# por país para cada región
datosONU_tidy_2007_reg %>% 
  ***(promedio_pob = ***/n_paises)
