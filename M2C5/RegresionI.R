library(ggplot2)
library(dplyr)
library(readr)

# Relaciones entre variables
str(mtcars)

mtcars %>% 
  summarise(covarianza = cov(hp, mpg))

mtcars %>% 
  summarise(correlacion = cor(hp, mpg))

# Estudiantes/Profesor vs Resultados
datos_colegio <- data.frame(REP = c(15, 17, 19, 20, 22, 23.5, 25), 
                            Resultados = c(680, 640, 670, 660, 630, 660, 635))
ggplot(datos_colegio, aes(REP, Resultados)) +
  geom_point(size = 3) +
  theme_minimal()

# El modelo más simple
ggplot(datos_colegio, aes(REP, Resultados)) +
  geom_point(size = 3) +
  geom_abline(aes(intercept = mean(Resultados), slope = 0),
              size = 2, col = "blue") +
  theme_minimal()

# Una línea que describe esta relación
ggplot(datos_colegio, aes(REP, Resultados)) +
  geom_point(size = 3) +
  geom_abline(aes(intercept = 713, slope = -3),
              size = 2, col = "blue") +
  theme_minimal()

# "Mejor" línea
ggplot(datos_colegio, aes(REP, Resultados)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm",
              se = FALSE,
              size = 2) +
  theme_minimal()

# Datos California (USA)
library(AER)
data("CASchools")
str(CASchools)

# Preparar datos
(datos_reg <- CASchools %>% 
    rename(ingresos = income) %>%
    transmute(distrito = district, colegio = school,
              Resultados = (read + math)/2,
              REP = students/teachers,
              ingresos,
              grupo_ingresos = as.factor(ifelse(ingresos >= median(ingresos), 1, 0)),
              computadores = computer,
              almuerzo = lunch))

# Relación entre variables
datos_reg %>% 
  ggplot(aes(REP, Resultados)) + 
  geom_point() +
  theme_minimal()

# Graficar la curva de regresión
datos_reg %>% 
  ggplot(aes(REP, Resultados)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()

# Estimar coeficientes "a mano"
(coeficientes <- datos_reg %>% 
    transmute(Y = Resultados,
              X = REP) %>% 
    summarise(beta_1 = cov(X,Y)/var(X),
              beta_0 = mean(Y)-(beta_1*mean(X))))

# Por suerte R lo hace más simple
modelo1 <- lm(Resultados ~ REP, data = datos_reg)
summary(modelo1)

# Calcular R2 "a mano"
datos_reg %>% 
  transmute(X = REP,
            Y = Resultados) %>%
  summarise(SCE = sum(((coeficientes$beta_0+(coeficientes$beta_1*X))-mean(Y))^2),
            SCT = sum((Y-mean(Y))^2),
            R2 = round(SCE/SCT, 5))

# Calcular R2 en R
summary(modelo1)

# Variable independiente no numérica
glimpse(datos_reg)

# Variable independiente no numérica
datos_reg %>% 
  group_by(grupo_ingresos) %>% 
  summarise(resultados_prom = mean(Resultados))

# Regresión con grupo_ingresos
(modelo2 <- lm(Resultados~grupo_ingresos, data = datos_reg))

# ¿Qué es esto?
str(modelo2)

# Paquete broom
library(broom)
tidy(modelo1)
glance(modelo1)
augment(modelo1)

# Analizar modelos
tidy(modelo1)
tidy(modelo2)

# Estimar coeficiente "a mano"
Y <- datos_reg %>% 
  select(Resultados) %>% 
  as.matrix()

X <- datos_reg %>% 
  mutate(intercepto = rep(1, nrow(.))) %>% 
  select(intercepto, REP, ingresos) %>% 
  as.matrix()

solve(t(X) %*% X) %*% (t(X) %*% Y)

# Por suerte R lo hace más simple
modelo3 <- lm(Resultados ~ REP + ingresos, data = datos_reg)
summary(modelo3)

# Interpretación
tidy(modelo3)

# Comparemos
glance(modelo1) %>% select(r.squared) %>% pull(1)
glance(modelo2) %>% select(r.squared) %>% pull(1)
glance(modelo3) %>% select(r.squared) %>% pull(1)

# R2 ajustado
glance(modelo1) %>% select(adj.r.squared) %>% pull(1)
glance(modelo2) %>% select(adj.r.squared) %>% pull(1)
glance(modelo3) %>% select(adj.r.squared) %>% pull(1)

# R2 no es la única métrica
glance(modelo1)
glance(modelo2)
glance(modelo3)

# Transformaciones de variables
modelo3 <- lm(Resultados ~ REP + ingresos, data = datos_reg)
tidy(modelo3)

datos_reg %>% 
  mutate(ingresos_nuevo = ingresos/10) %>% 
  lm(Resultados ~ REP + ingresos_nuevo, data = .) %>% 
  tidy()

# Transformaciones de variables
coef(lm(computadores ~ ingresos, data = datos_reg))

coef(lm(log(computadores) ~ ingresos, data = datos_reg))

coef(lm(computadores ~ log(ingresos), data = datos_reg))

coef(lm(log(computadores) ~ log(ingresos), data = datos_reg))

# Transformaciones de variables
coef(lm(log(computadores, base = exp(1)) ~ log(ingresos, base = exp(1)), data = datos_reg))

coef(lm(log(computadores, base = 10) ~ log(ingresos, base = 10), data = datos_reg))

coef(lm(log(computadores, base = 1234) ~ log(ingresos, base = 1234), data = datos_reg))

# Censo USA 2000
(census <- read_csv("../datos/census2000.csv") %>% 
    filter(hours > 500, income > 5000, age <60 ) %>% 
    mutate(ingresos_hora = income/hours) %>% 
    group_by(edad = age, sexo = sex) %>% 
    summarise(ingresos_hora = mean(ingresos_hora)) %>% 
    mutate(log_ingresos_hora = log(ingresos_hora)))

# Modelo simple
reg0 <- lm(ingresos_hora ~ edad, data = census)
tidy(reg0) %>% 
  select(1,2,5)

# Transformación Y
reg1 <- lm(log_ingresos_hora ~ edad, data = census)
tidy(reg1) %>% 
  select(1,2,5)

# Curvas por grupo
reg2 <- lm(log_ingresos_hora ~ edad + sexo, data = census)
tidy(reg2) %>% 
  select(1,2,5)

# Permitir pendientes distintas
reg3 <- lm(log_ingresos_hora ~ edad*sexo, data = census)
tidy(reg3) %>% 
  select(1,2,5)

# Ojo, se puede visualizar directamente
census %>% 
  ggplot(aes(x = edad, y = log_ingresos_hora, col = sexo)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()

# Capturar la curvatura
reg4 <- lm(log_ingresos_hora ~ edad*sexo + I(edad^2), data = census)
tidy(reg4) %>% 
  select(1,2,5)

# Permitir curvaturas distintas
reg5 <- lm(log_ingresos_hora ~ edad*sexo + I(edad^2)*sexo, data = census)
tidy(reg5) %>% 
  select(1,2,5)

# R2 ajustado
glance(reg1) %>% select(2)
glance(reg2) %>% select(2)
glance(reg3) %>% select(2)
glance(reg4) %>% select(2)
glance(reg5) %>% select(2)

# Estadístico F
anova(reg1, reg2)
anova(reg2, reg3)
anova(reg3, reg4)
anova(reg4, reg5)

# Criterios de información
(BIC <- data.frame(reg1 = glance(reg1) %>% pull(BIC), reg2 = glance(reg2) %>% pull(BIC),
                   reg3 = glance(reg3) %>% pull(BIC), reg4 = glance(reg4) %>% pull(BIC),
                   reg5 = glance(reg5) %>% pull(BIC)))
probs <- exp(-0.5*(BIC-min(BIC)))/sum(exp(-0.5*(BIC-min(BIC))))
round(probs, 3)

# Análisis de residuales
augment(reg0)