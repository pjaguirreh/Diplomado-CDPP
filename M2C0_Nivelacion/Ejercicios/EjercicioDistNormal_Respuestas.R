#-----------#
# Preguntas #
#-----------#

# Reemplace donde vea "***" con lo que considera correcto

# Asuma que la distribución de altura en una población sigue una distribución normal
# con media 177 cm y desviacion estandar 10 cm.

# ¿Cuál es la probabilidad de que alguien mida menos de 1.6 metros?
pnorm(160, mean = 177, sd = 10)

# ¿Cuál es la probabilidad de que alguien mida más de 1.8 metros?
1 - pnorm(180, 177, 10)
pnorm(180, 177, 10, lower.tail = FALSE)

# ¿Cuál es la probabilidad de que alguien mida entre 1.6 y 1.8 metros?
pnorm(180, 177, 10) - pnorm(160, 177, 10)
