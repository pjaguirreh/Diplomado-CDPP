######################
## Cargar librerías ##
######################
library(dplyr) # Manejo de datos
library(infer) # Inferencia estadística

##################
## Cargar datos ##
##################
# EJECUTE ESTE CÓDIGO
datos_satisfaccion <- c(rep("satisfecho", 146), rep("no satisfecho", 54)) %>%
  data.frame() %>%
  rename("satisfaccion" = 1) %>% 
  mutate(satisfaccion = as.factor(satisfaccion))

################
## Ejercicios ##
################

# El jefe de un servicio público dice que 80% de los ciudadanos que 
# ocupan alguno de sus servicios está satisfecho. Para probar esta hipótesis,
# un medio de comunicación encuesto aleatoriamente a 200 personas. 146 estaban
# satisfechas y 54 no. ¿Podemos rechazar la hipótesis del jefe del servicio?
# Consideremos un nivel de significancia del 5% (0.05)


# HIPÓTESIS NULA       : p = 0.8
# HIPÓTESIS ALTERNATIVA: p != 0.8 (p distinto a 0.8)

## Donde vea "***" es donde debe escribir algo

# Explore los datos
summary(datos_satisfaccion)

# Calcule el porcentaje de personas que reportan estar satisfechas
*** %>% 
  group_by(satisfaccion) %>% 
  summarise(n_casos = n()) %>% 
  ***(prop_casos = n_casos/sum(n_casos))

# Haga lo mismo usando las funciones del paquete "infer" y guarde el resultado
# en un objeto llamado "p_hat"
(p_hat <- *** %>% 
  specify(response = ***, success = "satisfecho") %>% 
  calculate(stat = "prop"))

# Simule una distribución asumiendo que la hipotesis nula (p = 0.8) es cierta
# guarde este resultado como "distribucion_nula
set.seed(2)
(*** <- datos_satisfaccion %>% 
  specify(response = ***, success = "satisfecho") %>% 
  hypothesise(null = "point", p = ***) %>% 
  generate(reps = 1000) %>% 
  calculate(stat = "prop"))

# Visualice "distribucion_nula"
distribucion_nula %>% 
  ***(bins = 10)

# Visualice el "p-value"
distribucion_nula %>% 
  ***(bins = 10) +
  shade_p_value(obs_stat = ***,
                direction = "both")

# Calcule el "p-vale" y asigne este valor al objeto "p_value"
(*** <- distribucion_nula %>% 
  get_p_value(obs_stat = ***, 
              direction = "both"))

# ¿Podemos rechazar la hipótesis nula? Considere el nivel de significancia
# mencionado al principio de este ejercicio
p_value<***

# Calcule el intervalo de confianza con un nivel de confianza de 95%
*** %>% 
  specify(response = ***, 
          success = "satisfecho") %>% 
  generate(reps = 1000) %>% 
  calculate(stat = "prop") %>% 
  get_confidence_interval(level = ***)

# Incluye este I.C. el valor correspondiente a la hipótesis nula?
0.8 %in% c(***, ***)
