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

################
## Ejercicios ##
################

## Donde vea "***" es donde debe escribir algo

# Cree un nuevo objeto "datosONU_tidy_new" donde elimine aquellas columnas que contengan la palabra
# "population" y que comiencen con la letra "f"
names(datosONU_tidy)

datosONU_tidy_new <- datosONU_tidy %>% 
  select(-contains("population"), -starts_with("f")) 

names(datosONU_tidy_new)

# Calcule el promedio para todas las variables numéricas (menos "year")
# Seleccione las columnas usando "across" y ":" (que ayuda a seleccionar un rango de columnas)
datosONU_tidy_new %>% 
  summarise(***(c(co2_emissions:public_spending_education), mean, na.rm = ***))

# Repita lo anterior pero utilizando la funcion ayuda "where".
datosONU_tidy_new %>% 
  ***(across(***(is.numeric) & !year, mean, na.rm = ***))

# Cree un nuevo objeto datosONU_tidy_new_summ realizando el mismo cálculo anterior pero
# para el año 2007 y grupando las observaciones por region e income_group
datosONU_tidy_new_summ <- datosONU_tidy_new %>% 
  ***(year == 2007) %>% 
  ***(region, income_group) %>% 
  ***(across(where(is.numeric) & !year, mean, na.rm = TRUE)) 

# Al objeto recién creado agregue una nueva columna correspondiente al log de "gdp_per_capita"
# guarde el objeto en uno nuevo llamado "datos_plot"
datos_plot <- datosONU_tidy_new_summ %>% 
  ***(log_gdp_per_capita = ***(gdp_per_capita))

# Genere un gráfico de dispersión (puntos) entre log_gdp_per_capita (x) y co2_emissions (y)
# Diferencio los puntos según su region
datos_plot %>% 
  ggplot(aes(x = ***, y = ***, col = ***)) +
  geom_***(size = 4) +
  theme_minimal() +
  labs(x = "Log de GDP per capita", y = "Emisiones de CO2")
