######################
## Cargar librerías ##
######################
library(readr) # Cargar datos
library(tidyr) # Manejo de datos
library(dplyr) # Manejo de datos

########################
## Cargar datos COVID ##
########################
## API creada por @pachamaltese para DATA UC (https://coronavirus.mat.uc.cl/)

# Casos totales por comuna incremental
covid_casos_region <- read_csv("https://coronavirus-api.mat.uc.cl/casos_totales_region_incremental")

# Fallecidos por región incremental
covid_muertes_region <- read_csv("https://coronavirus-api.mat.uc.cl/fallecidos_region_incremental")

# Exámenes PCR por región
covid_pcr_region <- read_csv("https://coronavirus-api.mat.uc.cl/examenes_pcr_region")

#########################
## Generar BBDD Región ##
#########################

# Crear BBDD
covid_datos_region <- covid_casos_region %>% 
  
  # cambio de nombre para que no se repitan
  rename(casos_covid = casos) %>% 
  
  # join de N° PCR con casos covid
  left_join(covid_pcr_region, by = c("fecha", "region")) %>% 
  
  # cambio de nombre para que no se repitan
  rename(n_pcr_dia = casos) %>% 
  
  # join de N° PCR y casos covid con muertes
  left_join(covid_muertes_region, by = c("fecha", "region")) %>% 
  
  # cambio de nombre para que no se repitan
  rename(muertes_covid = casos) %>%  
  
  # Reemplazamos NA con ceros en la columna de PCR
  replace_na(list(n_pcr_dia = 0)) %>% 
  
  # Agrupamos por región para relaizar la siguiente acción
  group_by(region) %>%
  
  # Imputamos las filas sin valores de población y codigo_region con los datos existentes
  fill(poblacion, .direction = "downup") %>% 
  fill(codigo_region, .direction = "downup") %>%

  # Crear nuevas variables
  mutate(pcr_acum = cumsum(n_pcr_dia), # número acumulado de examenes PCR
         pcr_acum_pob = (pcr_acum/poblacion)*10000,
         casos_covid_pob = (casos_covid/poblacion)*10000, # casos covid por cada 10.000 habitantes
         macroregion = case_when(
           codigo_region %in% c(15, 1, 2) ~ "Norte Grande",
           codigo_region %in% c(3, 4) ~ "Norte Chico",
           codigo_region %in% c(5, 13, 6, 7, 16, 8) ~ "Zona Central",
           codigo_region %in% c(9, 14, 10) ~ "Zona Sur",
           codigo_region %in% c(11, 12) ~ "Zona Austral"
         )
         ) %>% 
  select(region, macroregion, everything()) %>% 
 
  # Siempre es bueno "desagrupar" después de "agrupar"
  ungroup() %>% 
  
  filter(region != "Total")

# Exportar nueva BBDD regional en formato "csv"
write_csv(covid_datos_region, "datos/covid_datos_region.csv")
  
rm(covid_casos_region, covid_datos_region, covid_muertes_region, covid_pcr_region)
