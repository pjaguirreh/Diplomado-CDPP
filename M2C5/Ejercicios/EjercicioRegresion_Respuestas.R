######################
## Cargar librerías ##
######################
library(dplyr) # Manejo de datos
library(broom) # Manejo de datos de modelos
library(ggplot2) # Visualización de datos

##################
## Cargar datos ##
##################
datos_casas <- MASS::Boston %>% 
  transmute(valor_mediana = medv,
            antiguedad_casa = age,
            distancia_trabajo = log(dis),
            pob_vulnerable = log(lstat),
            nox, crim, dis, indus, tax)

names(datos_casas)

################
## Ejercicios ##
################

## Donde vea "***" es donde debe escribir algo

# Analice los datos "datos_casas"
summary(datos_casas)

# Cada observación (fila) corresponde a un barrio de Boston
# valor_mediana: mediana de precios de casas del barrio (en miles de USD)
# pob_vulnerable: medida de población adulta sin educación y/o con trabajos no calificados
# distancia_trabajo: medida de distancia/lejanía a "polos de trabajo" en Boston

# Genere un gráfico de pob_vulnerable (X) vs valor_mediana (Y)
# ¿Tiene sentido lo que observa?
datos_casas %>% 
  ggplot(aes(x = pob_vulnerable, y = valor_mediana)) +
  geom_point()

# Haga un modelo con "valor_mediana" como variable dependiente (Y) y "pob_vulnerable"
# como variable independiente (X). Guarde este modelo como "modelo_casas1".
# Analice los resultados. ¿Que puede decir respecto al coeficiente (beta) estimado 
# para "pob_vulnerable"? ¿es estadísticamente significativo?
modelo_casas1 <- lm(valor_mediana ~ pob_vulnerable, data = datos_casas)
tidy(modelo_casas1)
summary(modelo_casas1) #otra forma de explorar los resultados

#¿Cuál es el R2 ajustado de "modelo_casas1"?
glance(modelo_casas1)$adj.r.squared

# Genere un gráfico de distancia_trabajo (X) vs valor_mediana (Y)
# ¿Tiene sentido lo que observa?
datos_casas %>% 
  ggplot(aes(x = distancia_trabajo, y = valor_mediana)) +
  geom_point()

# Haga un modelo con "valor_mediana" como variable dependiente (Y) y "distancia_trabajo"
# como variable independiente (X). Guarde este modelo como "modelo_casas2".
# Analice los resultados. ¿Que puede decir respecto al coeficiente (beta) estimado 
# para "distancia_trbajo"? ¿es estadísticamente significativo?
modelo_casas2 <- lm(valor_mediana ~ distancia_trabajo, data = datos_casas)
tidy(modelo_casas2)

#¿Cuál es el R2 ajustado de "modelo_casas2"?
glance(modelo_casas2)$adj.r.squared

# Haga un modelo con "valor_mediana" como variable dependiente (Y) y "distancia_trabajo"
# y "pob_vulnerable"como variables independientes (Xs). Guarde este modelo como "modelo_casas3".
# Analice los resultados. ¿Que puede decir respecto al coeficiente (beta) estimado 
# para "distancia_trabajo"? ¿cambió la interpretación respecto a lo visto en "modelo_casas2"?
# ¿tiene este coeficiente más sentido?
modelo_casas3 <- lm(valor_mediana ~ pob_vulnerable + distancia_trabajo, data = datos_casas)
tidy(modelo_casas3)

#¿Cuál es el R2 ajustado de "modelo_casas2"?
glance(modelo_casas3)$adj.r.squared
