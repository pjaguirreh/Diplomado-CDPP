#-----------------#
# Generamos datos #
#-----------------#

tabla <- data.frame(
  "Narnia" = c(40, 210, 250),
  "Wakanda" = c(150, 350, 500),
  "Total" = c(190, 560, 750)
)
rownames(tabla) <- c("COVID_Si", "COVID_No", "Total")

tabla

#-------------------------#
# Calcular probabilidades #
#-------------------------#

# Para los ejercicios consideren que U (unión) y ^ es intersección
# Recuerden DataFrame[i,j] donde i es el número de la fila y j el número de la columna
# Reemplace donde vea "***" con lo que considera correcto

# EJEMPLO: ¿P(Narnia)?
tabla[3,1]/tabla[3,3] # P(Narnia)=0.33333

# ¿P(COVID_No)?
tabla[2,3]/tabla[3,3]

# ¿P(Narnia ^ COVID_No)?
tabla[2,1]/tabla[3,3]

# ¿P(COVID_Si|Narnia)?
tabla[1,1]/tabla[3,1]

# ¿P(Wakanda|COVID_No)?
tabla[2,2]/tabla[2,3]