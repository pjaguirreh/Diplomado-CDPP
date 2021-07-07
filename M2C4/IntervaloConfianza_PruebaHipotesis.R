library(readr)
library(dplyr)
library(tidyr)
library(infer)
library(ggplot2)

# Censo
censo <- read_csv("../datos/muestra_censo_2017.csv")
str(censo)

# Sabemos mu y sigma
(datos_poblacion <- censo %>% 
    summarise(promedio = mean(edad, na.rm = TRUE),
              sd = sd(edad, na.rm = TRUE)))

# Muestras
censo %>% 
  sample_n(100) %>% 
  summarise(promedio = mean(edad))

censo %>% 
  sample_n(100) %>% 
  summarise(promedio = mean(edad))

# Comparar distribuciones muestrales
guardar_mediacenso <- replicate(expr = mean(sample(censo$edad, 300)), n = 10000)

# Asumamos que no sabemos mu
set.seed(1) # para tener los mismos resultados
(muestra_censo <- censo %>% 
    sample_n(300) %>% 
    mutate(Id = row_number()) %>% 
    select(Id, edad))

# Explorar la muestra
muestra_censo %>% 
  ggplot(aes(x = edad)) +
  geom_histogram(color = "white")

(edad_promedio_muestra <- muestra_censo %>% 
    summarise(promedio_edad = mean(edad, na.rm = TRUE)))

# Bootstrapping
set.seed(5)
censo_muestra_r1 <- muestra_censo %>% 
  sample_n(300, 
           replace = TRUE)

# Muchas remuestras
(guardar_remuestras <- read_csv("../datos/remuestras_edad.csv"))

(promedio_remuestras <- 
    guardar_remuestras %>%
    group_by(remuestra) %>%
    summarise(promedio_edad = mean(edad, na.rm = TRUE)))

promedio_remuestras %>% 
  ggplot(aes(x = promedio_edad)) +
  geom_histogram(bins = 15, color = "white")

# Método percentiles
(metodo_percentiles <- promedio_remuestras %>% 
    summarise(percentil_2.5 = quantile(promedio_edad, 0.025), # Calcular percentil 2.5
              percentil_97.5 = quantile(promedio_edad, 0.975))) # Calcular percentil 97.5

promedio_remuestras %>% 
  ggplot(aes(x = promedio_edad)) +
  geom_histogram(bins = 15, color = "white") +
  geom_vline(xintercept = c(metodo_percentiles$percentil_2.5, 
                            metodo_percentiles$percentil_97.5), size = 1)

# Método error estándar
pnorm(1.96, mean = 0, sd = 1) - pnorm(-1.96, mean = 0, sd = 1)

(metodo_ee <- promedio_remuestras %>% 
    summarise(EE = sd(promedio_edad), # Calcular error estándar
              promedio = mean(promedio_edad)) %>%  # Calcular promedio
    mutate(lim_inf = promedio - (1.96*EE), # Calcular límite inferior I.C.
           lim_sup = promedio + (1.96*EE))) # Calcular límite superior I.C.

promedio_remuestras %>% 
  ggplot(aes(x = promedio_edad)) +
  geom_histogram(bins = 15, color = "white") +
  geom_vline(xintercept = c(metodo_percentiles$percentil_2.5, 
                            metodo_percentiles$percentil_97.5), size = 1) +
  geom_vline(xintercept = c(metodo_ee$lim_inf, 
                            metodo_ee$lim_sup), size = 1, color = "red", linetype = 2)

# Paquete infer
muestra_censo %>% 
  summarise(promedio = mean(edad))

muestra_censo %>% 
  specify(response = edad) %>% 
  calculate(stat = "mean")

muestra_censo %>% 
  specify(response = edad) %>% 
  generate(reps = 1000, 
           type = "bootstrap") %>% 
  calculate(stat = "mean") %>% 
  visualise()

# Construir I.C. con infer
set.seed(1)
guardar_remuestras_i <- muestra_censo %>% 
  specify(response = edad) %>% 
  generate(reps = 1000, 
           type = "bootstrap") %>% 
  calculate(stat = "mean")

## Método percentiles
guardar_remuestras_i %>% 
  get_confidence_interval(level = 0.95,
                          type = "percentile")

## Método error estándar
guardar_remuestras_i %>% 
  get_confidence_interval(level = 0.95,
                          type = "se",
                          point_estimate = edad_promedio_muestra)

# Error estándar de formas distintas

## Usando la dispersión de las remuestras (bootstrap)
promedio_remuestras %>% 
  summarise(sd = sd(promedio_edad, na.rm = TRUE)) %>% 
  transmute(error_est = sd)

## Usando la desviación estándar de la población
censo %>% 
  summarise(sd = sd(edad, na.rm = TRUE)) %>% 
  transmute(error_est = sd/sqrt(300))

## Usando la desviación estándar de la muestra
muestra_censo %>% 
  summarise(sd = sd(edad, na.rm = TRUE)) %>% 
  transmute(error_est = sd/sqrt(300))

# I.C. basado en teoría
muestra_censo %>% 
  summarise(mu = mean(edad, na.rm = TRUE),
            sd = sd(edad, na.rm = TRUE),
            formula_ee = sd/sqrt(300),
            IC_1 = mu - (1.96*formula_ee),
            IC_2 = mu + (1.96*formula_ee))

# Un caso real
ascensos <- read_csv("../datos/ascensos.csv")
glimpse(ascensos)

# Resultados
ascensos %>% 
  group_by(sexo, decision) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = sexo, y = n, fill = decision)) +
  geom_col() +
  labs(x = "Sexo en el CV")

# Mundo hipotético
ascensos_reordenado <- read_csv("../datos/ascensos_reordenado.csv")

ascensos_reordenado %>% 
  group_by(sexo, decision) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = sexo, values_from = n)

# Prueba de hipótesis con infer
set.seed(1)
(distribucion_nula <- ascensos %>% 
    specify(formula = decision ~ sexo, 
            success = "Con ascenso") %>% 
    hypothesise(null = "independence") %>% 
    generate(reps = 1000, 
             type = "permute") %>% 
    calculate(stat = "diff in props", 
              order = c("masculino", "femenino")))

(dif_observada <- ascensos %>% 
    specify(formula = decision ~ sexo, 
            success = "Con ascenso") %>% 
    calculate(stat = "diff in props", 
              order = c("masculino", "femenino")))

distribucion_nula %>% 
  visualise(bins = 10) +
  shade_p_value(obs_stat = dif_observada,
                direction = "right")

distribucion_nula %>% 
  get_p_value(obs_stat = dif_observada, 
              direction = "right")

# Prueba de hipótess vs I.C.
(distribucion_nula <- ascensos %>% 
    specify(formula = decision ~ sexo, 
            success = "Con ascenso") %>% 
    hypothesise(null = "independence") %>%
    generate(reps = 1000, 
             type = "permute") %>%
    calculate(stat = "diff in props", 
              order = c("masculino", "femenino")))

(distribucion_bootstrap <- ascensos %>% 
    specify(formula = decision ~ sexo, 
            success = "Con ascenso") %>% 
    #hypothesise(null = "independence") %>%
    generate(reps = 1000, 
             type = "bootstrap") %>%
    calculate(stat = "diff in props", 
              order = c("masculino", "femenino")))

(ic_percentil <- distribucion_bootstrap %>% 
    get_confidence_interval(level = 0.95, 
                            type = "percentile"))

distribucion_bootstrap %>% 
  visualise() +
  shade_confidence_interval(endpoints = ic_percentil)

# ¿Cómo usar esto?
muestra_censo %>% 
  summarise(promedio_muestra = mean(edad),
            s = sd(edad))

pt(2.446, df = 299, lower.tail = FALSE)*2
