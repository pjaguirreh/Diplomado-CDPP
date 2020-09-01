library(readr)
library(dplyr)
library(tidyr)
library(rvest)
library(stringr)
library(janitor)

#DatosONU <- read_csv("datos/DatosONU.csv")
#saveRDS(DatosONU, "datos/DatosONU.rds")

## DATOS ONU
#DatosONU %>%
#  filter(`Series Name` %in% c(
#    "Life expectancy at birth, total (years)",
#    "GDP per capita (constant 2005 US$)",
#    "CO2 emissions (metric tons per capita)",
#    "Public spending on education, total (% of government expenditure)",
#    "Urban population (% of total)",
#    "Population (Total)",
#    "Labor force participation rate, female (% of female population ages 15+) (modeled ILO estimate)",
#    "Poverty headcount ratio at $2 a day (PPP) (% of population)",
#    "Malnutrition prevalence, weight for age (% of children under 5)",
#    "Fertility rate, total (births per woman)",
#    "Forest area (% of land area)",
#    "Health expenditure per capita, PPP (constant 2005 international $)",
#    "Fossil fuel energy consumption (% of total)"
#  )) %>% 
#  #write_csv("datos/DatosONU_select.csv")
#  saveRDS("datos/DatosONU_select.rds")

# EXTRAER INFORMACION SOBRE REGION Y GRUPO DE INGRESOS DESDE WEB DEL BANCO MUNDIAL

url <- "https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups"

# DEFINIR FRAME PARA LA EXTRACCIÓN
# Los números hacen referencia a la posición de la información en el código de la web misma
nombres <- cbind(c(10, 13, 16, 19, 22, 25, 28, 32, 35, 39, 43),
                 c("East Asia and Pacific", "Europe and Central Afica", "Latin America and the Caribbean", "Middle East and North Africa",
                   "North America", "South Asia", "Sub-saharan Africa", "Low Income", "Lower Middle Income", "Upper Middle Income", "High Income")) %>% 
  tibble::as_tibble()

categoriapais <- data.frame() #almacenará la información

# Loop para extracción (web scraping) desde web del BM
# se necesita limpiar un poco la información que se extraer
for (i in 1:11){
  
  base <- read_html(url) %>% 
    rvest::html_nodes(paste0("table:nth-child(", nombres[[i, 1]], ") td")) %>% 
    str_remove_all("<td>") %>% 
    str_remove_all("<b>") %>%
    str_remove_all("</td>") %>% 
    str_remove_all("\n") %>% 
    str_remove_all("<td class=\"\"><b>") %>% 
    str_remove_all("</b>") %>% 
    str_remove_all("<td class=\"\">") %>% 
    tibble::as_tibble() %>% 
    rename(`Country Name` = value)
  
  # resultado de la iteración i
  base <- base %>% 
    mutate(categoria = rep(nombres[[i, 2]], nrow(base)))
  
  # resultado acumulado
  categoriapais <- bind_rows(categoriapais, base) %>% 
    filter(`Country Name` != "")
  
}

# ajustes de la información extraída para poder juntarla con la información del concurso ONU
categoriapais <- categoriapais %>% 
  mutate(`Country Name` = str_squish(`Country Name`),
         `Country Name` = recode(`Country Name`,
                          "Congo, Dem. Rep" = "Congo, Dem. Rep.",
                          "Congo, Rep" = "Congo, Rep.",
                          "Côte d'Ivoire" = "Cote d'Ivoire",
                          "Curaçao" = "Curacao",
                          "Eswatini" = "Swaziland",
                          "Faroe Islands" = "Faeroe Islands",
                          "Korea, Dem. People's Rep." = "Korea, Dem. Rep.",
                          "North Macedonia" = "Macedonia, FYR",
                          "São Tomé and Principe" = "Sao Tome and Principe"),
         `Country Name` = `Country Name`)

# Generar "base de datos" de nivel de ingreso
income_group <- categoriapais %>% 
  filter(categoria %in% c("Low Income", "Lower Middle Income", "Upper Middle Income", "High Income")) %>% 
  rename(`Income Group` = categoria)

# Generar "base de datos" de región del mundo
region <- categoriapais %>% 
  filter(!categoria %in% c("Low Income", "Lower Middle Income", "Upper Middle Income", "High Income")) %>% 
  rename(`Region` = categoria)

# JUNTAR Y EXPORTAR INFORMACIÓN

readRDS("datos/DatosONU_select.rds") %>% 
  left_join(income_group, by = "Country Name") %>% 
  left_join(region, by = "Country Name") %>% 
  clean_names() %>% 
  select(country_name, income_group, region, series_name, everything(), -x1, -series_code) %>% 
  write_csv("datos/DatosONU_notidy.csv")

readRDS("datos/DatosONU_select.rds") %>% 
  left_join(income_group, by = "Country Name") %>% 
  left_join(region, by = "Country Name") %>% 
  clean_names() %>% 
  select(-x1, -series_code) %>% 
  pivot_longer(x1972_yr1972:x2007_yr2007, names_to = "year", values_to = "value") %>% 
  mutate(year = as.numeric(str_sub(year, 9, 12))) %>% 
  pivot_wider(names_from = series_name, values_from = value) %>% 
  clean_names() %>% 
  select(country_name, income_group, region, year, everything()) %>% 
  write_csv("datos/DatosONU_tidy.csv")

rm(list = ls())
