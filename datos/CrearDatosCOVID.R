library(tidyverse)
library(lubridate)

read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto3/TotalesPorRegion.csv") %>% 
  pivot_longer(3:ncol(.), names_to = "Fecha", values_to = "Valor") %>% 
  filter(Categoria %in% c("Casos nuevos totales", "Fallecidos totales")) %>% 
  pivot_wider(names_from = Categoria, values_from = Valor) %>% 
  janitor::clean_names() %>% 
  rename(
    casos_nuevos = casos_nuevos_totales,
    fallecidos = fallecidos_totales
  ) %>% 
  group_by(region) %>% 
  mutate(fecha = as_date(fecha),
         fallecidos = fallecidos - lag(fallecidos, 1),
         fallecidos = ifelse(fallecidos < 0, 0, fallecidos),
         fallecidos_d = fallecidos,
         fallecidos = zoo::rollmean(fallecidos, k = 5, fill = NA),
         casos_nuevos_d = casos_nuevos,
         casos_nuevos = zoo::rollmean(casos_nuevos, k = 5, fill = NA)) %>% 
  ungroup() %>% 
  write_excel_csv("DatosCOVIDRegion.csv")


