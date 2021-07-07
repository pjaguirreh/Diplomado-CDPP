######################
## Cargar librerías ##
######################
library(readxl) # Cargar datos
library(readr) # Cargar datos
library(dplyr) # Manejo de datos
library(stringr) # Manejo de texto
library(ggplot2) # Visualización de datos

##################
## Cargar datos ##
##################

# SÓLO EJECUTEN ESTA PARTE DEL CÓDIGO #

datos_alcaldes <- read_xlsx("../datos/alcaldes_2021.xlsx") %>% 
  filter(!is.na(electo)) %>% 
  mutate(comuna = case_when(
    str_detect(comuna, "ñ") ~ str_replace_all(comuna, "ñ", "n"),
    str_detect(comuna, "Ñ") ~ str_replace_all(comuna, "Ñ", "N"),
    comuna == "Ñunoa" ~ "Nunoa",
    comuna == "Paihuano" ~ "Paiguano",
    comuna == "Trehuaco" ~ "Treguaco",
    comuna == "Marchigue" ~ "Marchihue",
    comuna == "O'higgins" ~ "Ohiggins",
    comuna == "Marchigue" ~ "Marchihue",
    TRUE ~ comuna
  ))

poblacion_comuna <- read_csv("../datos/PoblacionComuna.csv") %>% 
  mutate(comuna = str_to_title(comuna),
         comuna = case_when(
           comuna == "Antartica" ~ "Cabo De Hornos(Ex-Navarino) Y Antartica",
           comuna == "Cabo De Hornos" ~ "Cabo De Hornos(Ex-Navarino) Y Antartica",
           TRUE ~ comuna
         )) %>% 
  group_by(comuna) %>% 
  summarise(poblacion = sum(poblacion))

# ------------------ #
# Explorar los datos #
# ------------------ #

glimpse(datos_alcaldes)
glimpse(poblacion_comuna)

################
## Ejercicios ##
################

## Donde vea "***" es donde debe escribir algo

# ¿Cuántos candidatos a alcalde hubo en las últimas elecciones?
datos_alcaldes %>% 
  summarise(n_candidatos = n_distinct(candidato))

# ¿Cuántos candidatos a alcalde hubo por región? 
# ¿Qué región tuvo más?
datos_alcaldes %>% 
  group_by(region) %>% 
  summarise(n_candidatos = n_distinct(candidato)) %>% 
  arrange(-n_candidatos)

# ¿Cuántos candidatos a alcalde hubo por comuna en la RM? 
# ¿Qué región tuvo más?
datos_alcaldes %>% 
  filter(region == "Metropolitana Santiago") %>% 
  group_by(comuna) %>% 
  summarise(n_candidatos = n_distinct(candidato)) %>% 
  arrange(-n_candidatos)

# ¿Cuáles fueron los 5 partidos con más alcaldes ELECTOS?
datos_alcaldes %>% 
  filter(electo == TRUE) %>% 
  group_by(partido) %>% 
  summarise(n_electos = n_distinct(candidato)) %>% 
  arrange(-n_electos) %>% 
  slice(1:5)

# Calcule el total de votos por comuna y asigne esto a "resultados_comuna"
(resultados_comuna <- datos_alcaldes %>% 
    group_by(region, comuna) %>% 
    summarise(votos = sum(votos, na.rm = TRUE)))

# Cree un objeto "resultados_comuna_pob" uniendo las tablas "resultados_comuna" y
# "poblacion_comuna"
(resultados_comuna_pob <- resultados_comuna %>% 
    left_join(poblacion_comuna, by = "comuna"))

# Calcule el número de votos cada 10.000 habitantes para cada comuna
# ¿Cuáles son las 10 comunas con mayor número de votos cada 10.000 habitantes?
# Haga un gráfico de barras con esta información 
# (comuna en eje X y votos_pob en eje Y)
resultados_comuna_pob %>% 
  mutate(votos_pob = (votos/poblacion)*10000) %>% 
  arrange(-votos_pob) %>% 
  ungroup() %>% 
  slice(1:10) %>%
  ggplot(aes(x = reorder(comuna, votos_pob), y = votos_pob)) +
  geom_col() +
  coord_flip() +
  labs(
    x = NULL,
    y = NULL
  )

