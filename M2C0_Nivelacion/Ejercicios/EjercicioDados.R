#-----------------#
# Generamos datos #
#-----------------#

dado1 <- 1:6
dado2 <- 1:6
combinaciones <- expand.grid("dado1" = dado1, "dado2" = dado2)
combinaciones$suma_dados <- combinaciones$dado1 + combinaciones$dado2

#-----------#
# Preguntas #
#-----------#

# Reemplace donde vea "***" con lo que considera correcto

# Analice el objeto "combinaciones" ¿Cuál de las columnas es la variable aleatoria?
combinaciones

# Calcule la función de masa de probabilidad (pmf)
pmf <- table(combinaciones$***)/nrow(combinaciones)

# Confirme que la pmf suma 1
***(pmf)

# Grafique la pmf
plot(***)

# Calcule la función de distribución acumulada (cdf)
cdf <- cumsum(***)

# Grafique la cdf
***(cdf)

# ¿Cuál es el espacio muestral de la suma de dos datos?
espacio_muestral <- unique(combinaciones$***)

# ¿Cuál es el valor esperado de la suma de dos dados
e_x <- ***(espacio_muestral*pmf)

# ¿Cuál es la varianza y desviación estándar de la suma de dos datos?
varianza_dados <- (sum(espacio_muestral^2* ***))-(sum(e_x^2))
varianza_dados
sqrt(***)

