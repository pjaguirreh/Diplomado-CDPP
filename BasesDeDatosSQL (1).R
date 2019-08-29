library(dplyr)
library(DBI)
library(readxl)

#Ver la siguiente web para más información
#https://db.rstudio.com/

#3 EJEMPLOS DE CONEXIÓN A SQL (SQL Server en este caso)
#Estos ejemplos "no correrán" (darán error) ya que estoy inventando nombres de servidores y bases de datos. Los
#campos a completar son información que los administradores de bases de datos les deberían poder dar.

#Ejemplo 1
BBDDej1 <- dbConnect(odbc::odbc(),
                    encoding = 'windows-1252',
                    .connection_string = 'Driver={SQL Server};
                                          Server=ServidorX,
                                          1433;
                                          Database=BBDDY;
                                          trusted_connection=yes')

#Ejemplo 2
BBDDej2 <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "SQL Server",
                      Server   = "ServidorX",
                      Database = "BBDDY",
                      Trusted_Connection = "True",
                      Port     = 1433)

#Ejemplo 3
BBDDej3 <- DBI::dbConnect(odbc::odbc(),
                          Driver   = "SQL Server",
                          Server   = "ServidorX",
                          Database = "BBDDY",
                          UID      = rstudioapi::askForPassword("Database user"),
                          PWD      = rstudioapi::askForPassword("Database password"),
                          Port     = 1433)

dbDisconnect(BBDDej1) #Desconexión de la base de datos (siempre una buena práctica)

########
##DEMO##
########

source("BasesDeDatosSQL_EXTRA.R") #Simulación de una conexión a base de datos (este archivo tiene que estar en la misma carpeta)

dbListTables(con) #Muestra los objetos dentro de la base (hay dos bases: gsp y obesidad)

#Ejemplo de una tabla leída desde SQL
tbl(con, "obesidad") #Esta tabla "no está en R".. Asignenla a una variable y traten de ver cuantas filas tiene (usando nrow())

obesidad <- tbl(con, "obesidad") %>% #Importar datos
  rename("Income_Group" = "Income Group") %>% #Ajustar el nombre de la variable
  mutate(obesity_rate = as.numeric(obesity_rate)) %>% #Ajustar el tipo de dato
  filter(year %in% c(1995, 2000, 2005, 2010, 2015)) #Dejar datos para cinco años

## obesidad tampoco "está en R", por ahora es simplemente una llamad SQL

show_query(obesidad) #Podemos ver como se traduce de R a SQL lo que acabamos de escribir

gdp <- tbl(con, "gdp") %>% 
  select(-c(Indicator.Name, Indicator.Code)) %>% 
  rename("Code" = "Country.Code",
         "Country" = "Country.Name") %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year %in% c(1995, 2000, 2005, 2010, 2015))

#La función "collect()" ejecuta la llamada a SQL y "trae" la información a R (vean que ahora si se pueden ver las filas/columnas)
obesidad <- obesidad %>% collect
gdp <- gdp %>% collect


# Ahora bien, podemos hacer muchas operaciones en SQL (incluidos joins) y traer solo los resultados a R
obesidad <- tbl(con, "obesidad") %>% #Importar datos
  rename("Income_Group" = "Income Group") %>% #Ajustar el nombre de la variable
  mutate(obesity_rate = as.numeric(obesity_rate)) %>% #Ajustar el tipo de dato
  filter(year %in% c(1995, 2000, 2005, 2010, 2015)) #Dejar datos para cinco años

gdp <- tbl(con, "gdp") %>% 
  select(-c(Indicator.Name, Indicator.Code)) %>% 
  rename("Code" = "Country.Code",
         "Country" = "Country.Name") %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(year %in% c(1995, 2000, 2005, 2010, 2015))

datos <- left_join(obesidad, gdp, by = c("Code","year")) %>% 
  collect

dbDisconnect(con) #desconectar de la base de datos

