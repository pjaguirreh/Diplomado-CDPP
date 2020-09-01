######################
## Cargar librerías ##
######################
library(readr) # Cargar datos
library(ggplot2) # Visualizar datos
library(dplyr)

## Cargar datos
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

## LEFT JOIN
income_group <- datosONU_tidy %>% 
  distinct(country_name, income_group)

countries_noincomegroup <- datosONU_tidy %>% 
  select(country_name, year, fertility_rate)

# Digamos que queremos calcular el promedio de *fertility_rate* para cada *income_group* 
# pero nuestra tabla no tiene información sobre el grupo de ingresos
countries_noincomegroup

# Pero si tenemos otra tabla que asocia cada país a su grupo de ingresos
income_group

# Left join
countries_noincomegroup %>% 
  left_join(income_group, by = "country_name")

# ¿Y si los nombres no son iguales?
income_group2 <- datosONU_tidy %>% 
  distinct(country = country_name, income_group)

names(income_group2)
names(countries_noincomegroup)

countries_noincomegroup %>% 
  left_join(income_group2, by = c("country_name" = "country"))

## SET OPERATIONS
df1 <- datosONU_tidy %>% slice(1:10)
df2 <- datosONU_tidy %>% slice(5:15)

# La intersección corresponde a las filas 5, 6, 7, 8, 9, y 10 de la base original
intersect(df1, df2)

# La unión corresponde a las primeras 15 filas de la base original
union(df1, df2)

# Las filas que están en `df1` y no en `df2` corresponden a la 1, 2, 3, y 4 de la base original
setdiff(df1, df2)

# Las filas que están en `df2` y no en `df1` corresponden a la 11, 12, 13, 14, y 15 de la base original
setdiff(df2, df1)
