tabla <- data.frame(
  "Narnia" = c(40, 210, 250),
  "Wakanda" = c(150, 350, 500),
  "Total" = c(190, 560, 750)
)

rownames(tabla) <- c("COVID_Si", "COVID_No", "Total")

tabla

# Consideren que v es U (unión) y por ende ^ es interseccón

# P(Narnia)
tabla[3,1]/tabla[3,3]

# P(COVID_No)
tabla[2,3]/tabla[3,3]

# P(Narnia ^ COVID_No)
tabla[2,1]/tabla[3,3]
