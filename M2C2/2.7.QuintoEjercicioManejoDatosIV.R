######################
## Cargar librerías ##
######################
library(readr) # Cargar datos
library(dplyr) # Manejo de datos

##################
## Cargar datos ##
##################
datosONU_tidy <- read_csv("datos/DatosONU_tidy.csv") %>% 
  rename(
    "co2_emissions" = "co2_emissions_metric_tons_per_capita",
    "fertility_rate" = "fertility_rate_total_births_per_woman",
    "forest_area" = "forest_area_percent_of_land_area",
    "gdp_per_capita" = "gdp_per_capita_constant_2005_us",
    "health_expenditure" = "health_expenditure_per_capita_ppp_constant_2005_international",
    "labor_force_participation" = "labor_force_participation_rate_female_percent_of_female_population_ages_15_modeled_ilo_estimate",
    "life_expectancy" = "life_expectancy_at_birth_total_years",
    "malnutrition_prevalence" = "malnutrition_prevalence_weight_for_age_percent_of_children_under_5",
    "urban_population" = "urban_population_percent_of_total",
    "fossil_fuel_consumption" = "fossil_fuel_energy_consumption_percent_of_total",
    "poverty" = "poverty_headcount_ratio_at_2_a_day_ppp_percent_of_population",
    "public_spending_education" = "public_spending_on_education_total_percent_of_government_expenditure"
  )

paises_sindatos <- datosONU_tidy %>% 
  select(country_name, year)

regiones <- datosONU_tidy %>% 
  distinct(country_name, region)

grupo_ingresos <- datosONU_tidy %>% 
  distinct(country = country_name, income_group)

variables <- datosONU_tidy %>% 
  select(-income_group, -region) %>% 
  arrange(fertility_rate)

################
## Ejercicios ##
################

# Explore los siguientes objetos
paises_sindatos
regiones
grupo_ingresos
variables

## Donde vea "***" es donde debe escribir algo

# Haga un left join entre "paises_sindatos" y "regiones". ¿Qué columna tienen en común?
# Asigne el resultado al objeto "paises_regiones"
paises_regiones <- paises_sindatos %>% 
  left_***(regiones, by = "***") 

head(paises_regiones)

# Haga un left join del objeto recién creado con "grupo_ingresos". ¿Qué columna tienen en común? ¿tienen el mismo nombre?
# Asigne el resultado al objeto "paises_regiones_ingresos"
paises_regiones_ingresos <- paises_regiones %>% 
  ***(grupo_ingresos, by = c("***" = "***")) 

head(paises_regiones_ingresos)

# Haga un left join del objeto recién creado con "variables" ¿Qué columnas tienen en común?
# Noten que el join requiere especificar dos columnas en este caso
paises_regiones_ingresos_variables <- paises_regiones_ingresos %>% 
  ***(variables, by = c("***", "***"))

head(paises_regiones_ingresos_variables)

# ¿Qué países que estaban sobre la media de emisiones de co2 en 1987 lo seguían estando en 2007?
co2_sobreprom_1987 <- datosONU_tidy %>% 
  filter(*** == 1987,
         co2_emissions > ***(co2_emissions, na.rm = TRUE)) %>% 
  select(country_name)

co2_sobreprom_2007 <- datosONU_tidy %>% 
  filter(*** == 2007,
         co2_emissions > ***(co2_emissions, na.rm = TRUE)) %>% 
  select(country_name)

intersect(***, ***)
