library(moderndive)
library(patchwork)
library(tidyr)

ascensos <- read_csv("../datos/ascensos.csv")

ggplot(ascensos, aes(x = sexo, fill = decision)) +
  geom_bar() +
  labs(x = "Sexo de la persona") +
  scale_fill_manual(values = c("light green", "red"))

ascensos %>% 
  group_by(sexo) %>% 
  count(decision)

ascensos_reordenado <- read_csv("../datos/ascensos_reordenado.csv")

ggplot(ascensos, aes(x = sexo, fill = decision)) +
  geom_bar() +
  labs(x = "Sexo de la persona", title = "Original") +
  scale_fill_manual(values = c("light green", "red")) +
  theme(legend.position = "none")|
(
ggplot(ascensos_reordenado, aes(x = sexo, fill = decision)) +
  geom_bar() +
  labs(x = "Sexo de la persona", y = NULL, title = "Reordenado") +
  scale_fill_manual(values = c("light green", "red"))
)

ascensos_reordenado %>% 
  group_by(sexo) %>% 
  count(decision)

ascensos_reordenado_varios <- read_csv("../datos/ascensos_reordenado_varios.csv")

ascensos_reordenado_varios %>%
  pivot_longer(3:18, names_to = "persona", values_to = "resultado") %>% 
  group_by(decision, persona) %>% 
  count(resultado) %>% 
  ungroup() %>% 
  arrange(persona, resultado) %>% 
  group_by(persona, resultado) %>% 
  mutate(prop = n/sum(n)) %>% 
  ungroup() %>% 
  filter(decision == "con") %>% 
  select(persona, resultado, prop) %>% 
  pivot_wider(names_from = "resultado", values_from = prop) %>% 
  mutate(dif = m - f) %>% 
  ggplot(aes(dif)) +
  geom_histogram(bins = 8, color = "white") +
  geom_vline(xintercept = 0.2916667, size = 1, color = "red") +
  labs(x = "Diferencia en tasas de ascenso (masculino - femenino)")

distribucion_nula <- ascensos %>% 
  specify(formula = decision ~ sexo, success = "Con ascenso") %>% 
  hypothesise(null = "independence") %>% 
  generate(reps = 1000, type = "permute") %>% 
  calculate(stat = "diff in props", order = c("masculino", "femenino"))

distribucion_observada <- ascensos %>% 
  specify(formula = decision ~ sexo, success = "Con ascenso") %>% 
  calculate(stat = "diff in props", order = c("masculino", "femenino"))

visualise(distribucion_nula, bins = 10)

visualize(distribucion_nula, bins = 10) + 
  shade_p_value(obs_stat = distribucion_observada, direction = "right")

distribucion_nula %>% 
  get_p_value(obs_stat = distribucion_observada, direction = "right")

distribucion_bootstrap <- ascensos %>% 
  specify(formula = decision ~ sexo, success = "Con ascenso") %>% 
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "diff in props", order = c("masculino", "femenino"))

percentil_ic <- distribucion_bootstrap %>% 
  get_confidence_interval(level = 0.95, type = "percentile")
percentil_ic

visualize(distribucion_bootstrap) + 
  shade_confidence_interval(endpoints = percentil_ic)

ee_ic <- distribucion_bootstrap %>% 
  get_confidence_interval(level = 0.95, type = "se", 
                          point_estimate = distribucion_observada)
ee_ic

visualize(distribucion_bootstrap) + 
  shade_confidence_interval(endpoints = ee_ic)
