####################
##Importar datos 1##
####################

#Cargar librería readr
library("readr")

getwd()
setwd("C:\\Users\\pjagu\\Dropbox\\Diplomado Big data\\Versión 2019\\")

##leer archivo separado por "tabs"
txt  <- read_tsv("datos_originales/datos_txt.txt")
txt


(txt <- read_tsv("datos_originales/datos_txt.txt", 
                     col_types = cols(
                        A = col_character(),
                        B = col_character(),
                        C = col_character()
                                      )
                  )
)

##leer archivo con . como separador decimal y con columnas separadas por ,
csv <- read_csv("datos_originales/datos_csv.txt", col_names = FALSE)

##leer archivo con , como separador decimal y con columnas separadas po ;
csv2 <- read_csv("datos_originales/datos_csv2.txt", col_names = FALSE)
csv2 <- read_csv2("datos_originales/datos_csv2.txt")

##Algunas opciones que ser?n comunes para estas funciones
csv3 <- read_csv2("datos_originales/datos_csv2.txt", skip = 1)
csv3 <- read_csv2("datos_originales/datos_csv2.txt", col_names = FALSE)
csv3 <- read_csv2("datos_originales/datos_csv2.txt", skip = 1, col_names = c("V1", "V2", "V3"))

csv <- csv[-1,]
csv$X3 <- as.numeric(csv$X3)

##Leer archivos Excel (.xls o .xlsx)
library(readxl)

xlsx <- read_excel("datos_originales/datasets.xlsx")
xls <- read_excel("datos_originales/datasets.xls")

#Archivos Excel muchas veces tienen mas de una pesta?a. Por defecto se importa la primera
excel_sheets("datos_originales/datasets.xlsx")

#Importar otras pesta?as desde el mismo archivo Excel
mtcars_xlsx <- read_excel("datos_originales/datasets.xlsx", "mtcars")

quakes <- read_excel("datos_originales/datasets.xlsx", sheet = 4)

iris <- read_excel("datos_originales/datasets.xlsx", range = "iris!A1:C5")

##Leer archivos de otros software estad?sticos
library(haven)
stata <- read_stata("datos_originales/IMF.dta")

SPSS <- read_spss("datos_originales/survey.sav")

SAS <- read_sas("datos_originales/Canada.sas7bdat")

##exportar datos a csv
write_csv(stata, "datos_exportados/externos.csv")

##exportar datos a Stata/SPSS/SAS
write_sas(stata, "datos_exportados/STATAcomoSAS.sas7bdat")
write_dta(SPSS, "datos_exportados/SPSScomoSTATA.dta")
write_sav(stata, "datos_exportados/SAScomoSPSS")

##exportar a excel
library("openxlsx")
write.xlsx(stata, "stata.xlsx")
