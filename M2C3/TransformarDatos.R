######################
## Cargar librerías ##
######################
library(dplyr) # Verbos de manipulación de datos
library(tidyr) # Transformar datos
library(stringr) # Manejo de texto

# pivot_wider()
table2

table2 %>% 
  pivot_wider(names_from = type, 
              values_from = count)

# pivot_longer()
table4a

table4a %>% 
  pivot_longer(2:3, 
               names_to = "year", 
               values_to = "value")

# Demo - Tuberculosis

# Datos
glimpse(who)

(enfermedades <- who %>% 
    select(-iso2, -iso3))

names(enfermedades)

# Transformar forma
enfermedades %>% 
  pivot_longer(-c(country:year),       # Acá definimos que columnas hay que "pivotear"
               names_to = "variables", # Nombre de nueva columna donde irán los nombres de las columnas pivoteadas
               values_to = "valores")  # Nombre de nueva columna donde irán los valores correspondientes

# Eliminaremos parte de "variables"
enfermedades %>% 
  pivot_longer(-c(country:year), 
               names_to = "variables", 
               values_to = "valores") %>% 
  mutate(variables = str_remove(variables, # Eliminamos de la columna "variables"
                                "new_"),   # todo texto correspondiente a "new_"
         variables = str_remove(variables, # Eliminamos de la columna "variables"
                                "new"))    # todo texto correspondiente a "new"

# Separamos la columna "variables"
enfermedades %>% 
  pivot_longer(-c(country:year), 
               names_to = "variables", 
               values_to = "valores") %>% 
  mutate(variables = str_remove(variables, 
                                "new_"),
         variables = str_remove(variables, 
                                "new")) %>% 
  separate(variables,
           into = c("enfermedad", "otro"), # Nombre de nuevas columnas donde irán los valores de "variables"
           sep = "_")                      # Indicador de donde separar la columna

# Separamos la columna "otro"
enfermedades %>% 
  pivot_longer(-c(country:year), 
               names_to = "variables", 
               values_to = "valores") %>% 
  mutate(variables = str_remove(variables, 
                                "new_"),
         variables = str_remove(variables, 
                                "new")) %>% 
  separate(variables, 
           into = c("enfermedad", "otro"), 
           sep = "_") %>% 
  separate(otro,
           into = c("sexo", "edad"), # Nombre de nuevas columnas donde irán los valores de "otro"
           sep =  1)                 # Indicador de donde separar la columna

# Lo mismo usando paquete stringr
enfermedades %>% 
  pivot_longer(-c(country:year), 
               names_to = "variables", 
               values_to = "valores") %>% 
  mutate(variables = str_remove(variables, 
                                "new_"),
         variables = str_remove(variables, 
                                "new")) %>% 
  transmute(country, year, # transmute es similar a mutate pero solo dejará las columnas definidas en la función
            
            enfermedad = case_when(
              str_detect(variables, "rel") ~ str_sub(variables, 1, 3), # En los valores de "variables" donde detecte "rel", dejar solo el pedazo del valor corerspondiente a la primera, segunda y tercera letra
              TRUE ~ str_sub(variables, 1,2)),                         # En caso contrario, dejar la primera y segunda letra
            
            sexo = case_when(
              str_detect(variables, "m") ~ "m", # En los valores de "variables" donde se detecte "m", asignar el valor "m"
              TRUE ~ "f"),                      # En caso contrario, asignar "f"
            
            edad = str_extract(variables, "\\d+"), # Detectar donde hay un número, y extraer ese valor y todo lo que viene después
            valores)

# ¿transmute?
table2 %>% 
  pivot_wider(names_from = type, 
              values_from = count) %>% 
  mutate(casos_pop = (cases/population)*10000) # Deja todas las columnas y suma "casos_pop"

table2 %>% 
  pivot_wider(names_from = type, 
              values_from = count) %>% 
  transmute(casos_pop = (cases/population)*10000) # Solo deja "casos_pop"

# ¿case_when?
table2 %>% 
  pivot_wider(names_from = type, 
              values_from = count) %>% 
  mutate(indicador = ifelse(year == 1999, 
                            1, 
                            0))

table2 %>% 
  pivot_wider(names_from = type, 
              values_from = count) %>% 
  mutate(indicador = case_when(
    year == 1999 ~ 1, # Si el valor de "year" es igual a 1999, la variable indicador será 1
    year != 1999 ~ 0  # Si el valor de "year" es distinto a 1999, variable indicador será 0
  ))

table2 %>% 
  pivot_wider(names_from = type, 
              values_from = count) %>% 
  mutate(indicador = case_when(
    year == 1999 ~ 1, # Si el valor de "year" es igual a 1999, la variable indicador será 1
    TRUE ~ 0          # En caso contrario, indicador será 0
  ))

table2 %>% 
  pivot_wider(names_from = type, 
              values_from = count) %>% 
  mutate(indicador = ifelse(year == 1999, 
                            1,
                            ifelse(country == "Brazil", 
                                   2, 
                                   0)))

table2 %>% 
  pivot_wider(names_from = type, 
              values_from = count) %>% 
  mutate(indicador = case_when(
    year == 1999 ~ 1,
    country == "Brazil" ~ 2,
    TRUE ~ 0
  ))

# Cambios para más entendimiento
enfermedades %>% 
  pivot_longer(-c(country:year), 
               names_to = "variables", 
               values_to = "valores") %>% 
  mutate(variables = str_remove(variables, 
                                "new_"), 
         variables = str_remove(variables, 
                                "new")) %>% 
  separate(variables, 
           into = c("enfermedad", "otro"), 
           sep = "_") %>% 
  separate(otro,
           into = c("sexo", "edad"),
           sep =  1) %>% 
  mutate(
    edad = case_when(            # Recodificar la variable edad
      edad == "014" ~ "0-14",
      edad == "1524" ~ "15-24",
      edad == "2534" ~ "25-34",
      edad == "3544" ~ "35-44",
      edad == "4554" ~ "45-54",
      edad == "5564" ~ "55-64",
      edad == "65" ~ "65+"),
    
    sexo = case_when(            # Recodificar la variable sexo
      sexo == "m" ~ "hombres",
      sexo == "f" ~ "mujeres"))

# Total por sexo/edad para 2010
enfermedades %>% 
  pivot_longer(-c(country:year), 
               names_to = "variables", 
               values_to = "valores") %>% 
  mutate(variables = str_remove(variables, 
                                "new_"), 
         variables = str_remove(variables, 
                                "new")) %>% 
  separate(variables, 
           into = c("enfermedad", "otro"), 
           sep = "_") %>% 
  separate(otro,
           into = c("sexo", "edad"),
           sep =  1) %>% 
  mutate(
    edad = case_when(
      edad == "014" ~ "0-14",
      edad == "1524" ~ "15-24",
      edad == "2534" ~ "25-34",
      edad == "3544" ~ "35-44",
      edad == "4554" ~ "45-54",
      edad == "5564" ~ "55-64",
      edad == "65" ~ "65+"), 
    sexo = case_when( 
      sexo == "m" ~ "hombres", 
      sexo == "f" ~ "mujeres")) %>% 
  filter(year == 2010) %>%                      # Dejamos solo observaciones para el año 2010
  group_by(sexo, edad) %>%                      # Agrupamos por sexo y edad
  summarise(total = sum(valores, na.rm = TRUE)) # Y para cada grupo sumamos la columna "valores"

# Tabla final
enfermedades %>% 
  pivot_longer(-c(country:year), 
               names_to = "variables", 
               values_to = "valores") %>% 
  mutate(variables = str_remove(variables, 
                                "new_"), 
         variables = str_remove(variables, 
                                "new")) %>% 
  separate(variables, 
           into = c("enfermedad", "otro"), 
           sep = "_") %>% 
  separate(otro,
           into = c("sexo", "edad"),
           sep =  1) %>% 
  mutate(
    edad = case_when(
      edad == "014" ~ "0-14",
      edad == "1524" ~ "15-24",
      edad == "2534" ~ "25-34",
      edad == "3544" ~ "35-44",
      edad == "4554" ~ "45-54",
      edad == "5564" ~ "55-64",
      edad == "65" ~ "65+"), 
    sexo = case_when( 
      sexo == "m" ~ "hombres", 
      sexo == "f" ~ "mujeres")) %>% 
  filter(year == 2010) %>% 
  group_by(sexo, edad) %>% 
  summarise(total = sum(valores, na.rm = TRUE)) %>% 
  pivot_wider(names_from = edad,   # Tomamos los valores únicos de "edad" y los usaremos como nombres para las nuevas columnas
              values_from = total) # Rellenaremos la tabla con los valores de la columna "total"
