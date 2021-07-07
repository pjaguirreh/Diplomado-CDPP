library(readr)
library(dplyr)
library(ggplot2)

# Censo
censo <- read.csv("../datos/muestra_censo_2017.csv")
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

# 10.0000 muestras con n = 100
guardar_mediacenso <- replicate(expr = mean(sample(censo$edad, 100)), n = 10000)

edad_p <- censo %>% 
  summarise(promedio_edad = mean(edad))


## INTERVALO DE CONFIANZA

# ¿Cuál es la edad promedio en Chile?
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
    summarise(promedio_edad = mean(edad, 
                                   na.rm = TRUE)))

# Bootstrapping
set.seed(5)
censo_muestra_r1 <- muestra_censo %>% 
  sample_n(300, replace = TRUE)

# Muchas remuestras
guardar_remuestras <- read_csv("../datos/remuestras_edad.csv")
guardar_remuestras

(promedio_remuestras <- 
    guardar_remuestras %>%
    group_by(remuestra) %>%
    summarise(promedio_edad = mean(edad,
                                   na.rm = TRUE)))

promedio_remuestras %>% 
  ggplot(aes(x = promedio_edad)) +
  geom_histogram(bins = 15, color = "white")

# Método de percentiles
(metodo_percentiles <- promedio_remuestras %>% 
    summarise(percentil_2.5 = quantile(promedio_edad, 0.025), # Calcular percentil 2.5
              percentil_97.5 = quantile(promedio_edad, 0.975))) # Calcular percentil 97.5

promedio_remuestras %>% 
  ggplot(aes(x = promedio_edad)) +
  geom_histogram(bins = 15, color = "white") +
  geom_vline(xintercept = c(metodo_percentiles$percentil_2.5, 
                            metodo_percentiles$percentil_97.5),
             size = 1)

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
                            metodo_percentiles$percentil_97.5),
             size = 1) +
  geom_vline(xintercept = c(metodo_ee$lim_inf, 
                            metodo_ee$lim_sup),
             size = 1, color = "red", linetype = 2)


# Paquete infer
muestra_censo %>% 
  summarise(promedio = mean(edad))

library(infer)
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

# Método percentiles
guardar_remuestras_i %>% 
  get_confidence_interval(level = 0.95,
                          type = "percentile")


# Método error estándar
guardar_remuestras_i %>% 
  get_confidence_interval(level = 0.95,
                          type = "se",
                          point_estimate = edad_promedio_muestra)










set.seed(10)
muestra_censo <- censo %>% 
  sample_n(100)

edad_p_m <- muestra_censo %>% 
  summarise(promedio_edad = mean(edad))

set.seed(5)
censo_muestra_r1 <- muestra_censo %>% 
  sample_n(100, replace = TRUE)

ggplot(muestra_censo, aes(x = edad)) +
  geom_histogram(color = "white") +
  labs(title = "Muestra original de 300 observaciones")
ggplot(censo_muestra_r1, aes(x = edad)) +
  geom_histogram(color = "white") +
  labs(title = "Remuestra de 300 observaciones")

edad_p_r1 <- censo_muestra_r1 %>% 
  summarise(promedio_edad = mean(edad))

guardar_remuestras <- data.frame()
set.seed(1)
for (i in 1:1000){
  
  x <- muestra_censo %>% 
    sample_n(300, replace = TRUE) %>% 
    mutate(remuestra = i)
  
  guardar_remuestras <- bind_rows(guardar_remuestras, x)
  
}
guardar_remuestras %>% 
  select(-1) %>% 
  write_csv("../datos/remuestras_edad.csv")

#library(infer)
#set.seed(1)
#muestra_censo %>% 
#  specify(response = edad) %>% 
#  generate(reps = 100) %>% 
#  calculate(stat = "mean")

promedio_resmuestras <- guardar_remuestras %>% 
  group_by(remuestra) %>% 
  summarise(promedio_edad = mean(edad))

promedio_resmuestras %>% 
  ggplot(aes(x = promedio_edad)) +
  geom_histogram(bins = 15, color = "white")

metodo_percentiles <- promedio_resmuestras %>% 
  summarise(percentil_2.5 = quantile(promedio_edad, 0.025),
            percentil_97.5 = quantile(promedio_edad, 0.975))

# Método de percentil
promedio_resmuestras %>% 
  ggplot(aes(x = promedio_edad)) +
  geom_histogram(bins = 15, color = "white") +
  geom_vline(xintercept = c(metodo_percentiles$percentil_2.5, 
                            metodo_percentiles$percentil_97.5),
             size = 1)

# Método de error estándar
metodo_ee <- promedio_resmuestras %>% 
  summarise(EE = sd(promedio_edad),
            promedio = mean(promedio_edad)) %>% 
  mutate(lim_inf = promedio - (1.96*EE),
         lim_sup = promedio + (1.96*EE))


# Método de percentil
promedio_resmuestras %>% 
  ggplot(aes(x = promedio_edad)) +
  geom_histogram(bins = 15, color = "white") +
  geom_vline(xintercept = c(metodo_percentiles$percentil_2.5, 
                            metodo_percentiles$percentil_97.5),
             size = 1) +
  geom_vline(xintercept = c(metodo_ee$lim_inf, 
                            metodo_ee$lim_sup),
             size = 1, color = "red", linetype = 2)



#
library(infer)
set.seed(1)
guardar_remuestras_i <- muestra_censo %>% 
  specify(response = edad) %>% 
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "mean")


visualize(guardar_remuestras_i)

intervalo_percentil <- guardar_remuestras_i %>% 
  get_confidence_interval(level = 0.95, type = "percentile")

visualize(guardar_remuestras_i) + 
  shade_ci(endpoints = intervalo_percentil)

intervalo_ee <- guardar_remuestras_i %>% 
  get_confidence_interval(point_estimate = edad_p_m, type = "se")

visualize(guardar_remuestras_i) + 
  shade_ci(endpoints = intervalo_ee)


guardar_95 <- data.frame()
set.seed(4)
for (i in 1:100){
  
  x <- censo %>% 
    sample_n(100) %>% 
    specify(response = edad) %>% 
    generate(reps = 1000, type = "bootstrap") %>% 
    calculate(stat = "mean") 
  
  x_med <- x %>% 
    summarise(med = mean(stat))
  
  x_ci <- x %>% 
    get_confidence_interval(level = 0.95, type = "percentile") %>% 
    mutate(rep = i,
           med = x_med$med, .before = 1)
  
  guardar_95 <- bind_rows(guardar_95, x_ci)
}

resultado_ci_95 <- guardar_95 %>% 
  mutate(contiene = ifelse(lower_ci <= edad_p$promedio_edad & upper_ci >= edad_p$promedio_edad, 
                           "si", "no")) 

resultado_ci_95 %>%
  ggplot() +
  geom_errorbar(aes(x = rep, 
                    ymin = lower_ci, 
                    ymax = upper_ci,
                    color=contiene), width=.1) +
  geom_point(aes(x = rep, y = med, color = contiene), size = 0.7) +
  geom_hline(yintercept = edad_p$promedio_edad) +
  coord_flip() +
  theme_minimal() +
  labs(x = "N° de intervalo de confianza",
       y = "Edad") +
  scale_color_manual(values = c("red", "grey"))

guardar_90 <- data.frame()
set.seed(5)
for (i in 1:100){
  
  x <- censo %>% 
    sample_n(100) %>% 
    specify(response = edad) %>% 
    generate(reps = 1000, type = "bootstrap") %>% 
    calculate(stat = "mean") 
  
  x_med <- x %>% 
    summarise(med = mean(stat))
  
  x_ci <- x %>% 
    get_confidence_interval(level = 0.9, type = "percentile") %>% 
    mutate(rep = i,
           med = x_med$med, .before = 1)
  
  guardar_90 <- bind_rows(guardar_90, x_ci)
}

resultado_ci_90 <- guardar_90 %>% 
  mutate(contiene = ifelse(lower_ci <= edad_p$promedio_edad & upper_ci >= edad_p$promedio_edad, 
                           "si", "no")) 

resultado_ci_90 %>%
  ggplot() +
  geom_errorbar(aes(x = rep, 
                    ymin = lower_ci, 
                    ymax = upper_ci,
                    color=contiene), width=.1) +
  geom_point(aes(x = rep, y = med, color = contiene), size = 0.7) +
  geom_hline(yintercept = edad_p$promedio_edad) +
  coord_flip() +
  theme_minimal() +
  labs(x = "N° de intervalo de confianza",
       y = "Edad") +
  scale_color_manual(values = c("red", "grey"))



guardar_95_100 <- data.frame()
set.seed(4)
for (i in 1:100){
  
  x <- censo %>% 
    sample_n(100) %>% 
    specify(response = edad) %>% 
    generate(reps = 1000, type = "bootstrap") %>% 
    calculate(stat = "mean") 
  
  x_med <- x %>% 
    summarise(med = mean(stat))
  
  x_ci <- x %>% 
    get_confidence_interval(level = 0.95, type = "percentile") %>% 
    mutate(rep = i,
           med = x_med$med, .before = 1)
  
  guardar_95_100 <- bind_rows(guardar_95_100, x_ci)
}

resultado_ci_95_100 <- guardar_95_100 %>% 
  mutate(contiene = ifelse(lower_ci <= edad_p$promedio_edad & upper_ci >= edad_p$promedio_edad, 
                           "si", "no")) 

guardar_95_500 <- data.frame()
set.seed(4)
for (i in 1:100){
  
  x <- censo %>% 
    sample_n(500) %>% 
    specify(response = edad) %>% 
    generate(reps = 1000, type = "bootstrap") %>% 
    calculate(stat = "mean") 
  
  x_med <- x %>% 
    summarise(med = mean(stat))
  
  x_ci <- x %>% 
    get_confidence_interval(level = 0.95, type = "percentile") %>% 
    mutate(rep = i,
           med = x_med$med, .before = 1)
  
  guardar_95_500 <- bind_rows(guardar_95_500, x_ci)
}

resultado_ci_95_500 <- guardar_95_500 %>% 
  mutate(contiene = ifelse(lower_ci <= edad_p$promedio_edad & upper_ci >= edad_p$promedio_edad, 
                           "si", "no")) 


guardar_95_1000 <- data.frame()
set.seed(4)
for (i in 1:100){
  
  x <- censo %>% 
    sample_n(1000) %>% 
    specify(response = edad) %>% 
    generate(reps = 1000, type = "bootstrap") %>% 
    calculate(stat = "mean") 
  
  x_med <- x %>% 
    summarise(med = mean(stat))
  
  x_ci <- x %>% 
    get_confidence_interval(level = 0.95, type = "percentile") %>% 
    mutate(rep = i,
           med = x_med$med, .before = 1)
  
  guardar_95_1000 <- bind_rows(guardar_95_1000, x_ci)
}

resultado_ci_95_1000 <- guardar_95_1000 %>% 
  mutate(contiene = ifelse(lower_ci <= edad_p$promedio_edad & upper_ci >= edad_p$promedio_edad, 
                           "si", "no")) 

resultado_ci_95_100 %>%
  ggplot() +
  geom_errorbar(aes(x = rep, 
                    ymin = lower_ci, 
                    ymax = upper_ci), width=.1) +
  geom_point(aes(x = rep, y = med), size = 0.7) +
  geom_hline(yintercept = edad_p$promedio_edad) +
  coord_flip() +
  theme_minimal() +
  labs(x = "N° de intervalo de confianza",
       y = "Edad") +
  ylim(c(25,50))

resultado_ci_95_500 %>%
  ggplot() +
  geom_errorbar(aes(x = rep, 
                    ymin = lower_ci, 
                    ymax = upper_ci), width=.1) +
  geom_point(aes(x = rep, y = med), size = 0.7) +
  geom_hline(yintercept = edad_p$promedio_edad) +
  coord_flip() +
  theme_minimal() +
  labs(x = "N° de intervalo de confianza",
       y = "Edad") +
  ylim(c(25,50))

resultado_ci_95_1000 %>%
  ggplot() +
  geom_errorbar(aes(x = rep, 
                    ymin = lower_ci, 
                    ymax = upper_ci), width=.1) +
  geom_point(aes(x = rep, y = med), size = 0.7) +
  geom_hline(yintercept = edad_p$promedio_edad) +
  coord_flip() +
  theme_minimal() +
  labs(x = "N° de intervalo de confianza",
       y = "Edad") +
  ylim(c(25,50))
