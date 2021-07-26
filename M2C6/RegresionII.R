library(dplyr)
library(ggplot2)
library(broom)

# Variable dependiente categórica
set.seed(1)
X1 <- sample(1:11, 100, replace = TRUE)
X2 <- sample(10:20, 100, replace = TRUE)

datos_logit <- bind_rows(data.frame(X = X1, Y = rep(0, 100)),
                         data.frame(X = X2, Y = rep(1, 100))) 

# Modelo de probabilidad lineal
lm(Y~X, data = datos_logit)

# Modelo logit
(modelo_logit <- glm(Y ~ X, family = "binomial", data = datos_logit))

# Con datos reales
# Datos laborales
(datos_trabajo <- read_xlsx("../datos/datos_trabajo.xlsx"))

# Visualicemos los datos
datos_trabajo %>% 
  ggplot(aes(x = educ, y = trabajando)) +
  geom_point() +
  theme_minimal()

# Un pequeño ajuste
datos_trabajo %>% 
  ggplot(aes(x = educ, y = trabajando)) +
  geom_jitter(width = 0.1, height = 0.03, size = 0.3) +
  theme_minimal()

# Modelo de probabilidad lineal
datos_trabajo %>% 
  ggplot(aes(x = educ, y = trabajando)) +
  geom_jitter(width = 0.1, height = 0.03, size = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()

# Modelo logit
datos_trabajo %>% 
  ggplot(aes(x = educ, y = trabajando)) +
  geom_jitter(width = 0.1, height = 0.03, size = 0.3) +
  geom_smooth(method = "glm", se = FALSE, method.args = list(family = "binomial")) +
  theme_minimal()

# Modelo logit con otra variable
datos_trabajo %>% 
  ggplot(aes(x = exper, y = trabajando)) +
  geom_jitter(width = 0.1, height = 0.03, size = 0.3) +
  geom_smooth(method = "glm", se = FALSE, method.args = list(family = "binomial")) + 
  theme_minimal()

# Estimemos un modelo logit
modelo_logit_trabajo <- glm(trabajando ~ educ, 
                            family = "binomial", 
                            data = datos_trabajo)
tidy(modelo_logit_trabajo)

# Interpretación
nuevos_datos <- data.frame("educ" = c(5, 7, 9, 11, 13, 15, 17))
predict(modelo_logit_trabajo,
        newdata = nuevos_datos,
        type = "response") %>% round(4)

# Pseudo R-2
glance(modelo_logit_trabajo)

1-(select(glance(modelo_logit_trabajo), deviance)/select(glance(modelo_logit_trabajo), null.deviance)) %>% pull()

# ¿Cómo evaluamos este modelo?
summary(modelo_logit_trabajo)

(estimacion_logit <- augment(modelo_logit_trabajo, type.predict = "response") %>% 
    transmute(valor_real = trabajando,
              .fitted,
              valor_estimado = ifelse(.fitted >= 0.5, 1, 0),
              check = valor_real == valor_estimado))

# Matriz de confusión
(matriz_confusion <- estimacion_logit %>% 
    group_by(valor_real, valor_estimado) %>% 
    summarise(n = n()) %>% 
    pivot_wider(names_from = valor_estimado, values_from = n))

VP <- matriz_confusion[2,3]
FP <- matriz_confusion[1,3]
VN <- matriz_confusion[1,2]
FN <- matriz_confusion[2,2]
(tasa_VP <- VP/(VP+FN))
(tasa_FP <- FP/(FP+VN))