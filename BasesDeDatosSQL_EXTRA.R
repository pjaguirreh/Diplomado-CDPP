#Este es un archivo complementario a "BasesDeDatosSQL.R" (este script se ejecuta desde ahí)
#Ambos archivos tienen que estar en la misma carpeta para que corra bien el código (si no hay que especificar bien la ruta)

con <- dbConnect(RSQLite::SQLite(), dbname = ":memory:")

#https://cran.r-project.org/web/packages/dbplyr/vignettes/dbplyr.html

obesidad <- read_csv("datos_originales/Obesity_both_9016.csv") %>% #Importar datos
  gather(year, obesity_rate, "year_1990":"year_2016") %>% #Cambiar de datos "anchos" a "largos"
  separate(year, into = c(NA, "year"), convert = TRUE)
gdp <- as_tibble(read_xlsx("datos_originales/GDPpercapPPP.xlsx")) %>% 
  gather(year, gdp, "1990":"2017") 

copy_to(con, obesidad, "obesidad",
        temporary = FALSE
)
copy_to(con, gdp, "gdp",
        temporary = FALSE
)

rm(obesidad, gdp)