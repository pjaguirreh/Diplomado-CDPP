######################
## Cargar librerías ##
######################
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# Pivot_wider
table2

table2 %>% 
  pivot_wider(names_from = type, 
              values_from = count)

# Pivot_longer
table4a

table4a %>% 
  pivot_longer(2:3, 
               names_to = "year", 
               values_to = "value")

## DEMO - Enfermedades respiratorios

# Inspeccionar datos
glimpse(who)

# 
enfermedades <- who %>% 
    select(-iso2, -iso3)
enfermedades

# Transformamos la forma de nuestros datos
enfermedades2 <- enfermedades %>% 
    pivot_longer(-c(country:year), 
                 names_to = "variables", 
                 values_to = "valores")
enfermedades2

# Eliminaremos parte de la columna "variables" que no nos sirve
enfermedades3 <- enfermedades2 %>% 
    mutate(variables = str_remove(variables, "new_"),
           variables = str_remove(variables, "new"))
enfermedades3

# Separate para extraer las variables concatenadas (i)
enfermedades4 <- enfermedades3 %>% 
    separate(variables, 
             into = c("enfermedad", "otro"), 
             sep = "_")
enfermedades4

# Separate para extraer las variables concatenadas (ii)
enfermedades5 <- enfermedades4 %>% 
    separate(otro, 
             into = c("sexo", "edad"), 
             sep =  1)
enfermedades5

# Se podría llegar a lo mismo usando stringr
enfermedades3 %>% 
  transmute(country, year,
            enfermedad = case_when(
              str_detect(variables, "rel") ~ str_sub(variables, 1, 3),
              TRUE ~ str_sub(variables, 1,2)),
            sexo = case_when(
              str_detect(variables, "m") ~ "m",
              TRUE ~ "f"),
            edad = str_extract(variables, "\\d+"),
            valores
  )

# Modificaremos algunos valores para mayor entendimiento
enfermedades6 <- enfermedades5 %>% 
    mutate(
      edad = case_when(
        edad == "014" ~ "0-14",
        edad == "1524" ~ "15-24",
        edad == "2534" ~ "25-34",
        edad == "3544" ~ "35-44",
        edad == "4554" ~ "45-54",
        edad == "5564" ~ "55-64",
        edad == "65" ~ "65+"
      ),
      sexo = case_when(
        sexo == "m" ~ "hombres",
        sexo == "f" ~ "mujeres"
      ))
enfermedades6

# Calculamos el total de enfermedades por sexo y rango de edad
resumen_enfermedades <- enfermedades6 %>% 
    filter(year == 2010) %>% 
    group_by(sexo, edad) %>% 
    summarise(total = sum(valores, na.rm = TRUE))
resumen_enfermedades

# Pivot_wider para dar la forma final a la tabla
tabla_final <- resumen_enfermedades %>% 
    pivot_wider(names_from = edad, values_from = total)
tabla_final
