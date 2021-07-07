## Complemento de un evento

# Definir tres *vectores* $A$, $B$, $C$
 
A <- c(2,4,6)
B <- c(1,3,5)
C <- c(2,3,5)

# Definir un vector que contiene los elementos de $A$ y $B$
AyB <- c(A, B)

# Ver que elementos que están en $A$ o $B$, no están en $C$
setdiff(AyB, C)

## Eventos mutuamente excluyentes

# ¿Existe al menos un elemento de $A$ igual a un elemento en $C$?
intersect(A, C)

# ¿Existe al menos un elemento de $B$ igual a un elemento en $C$?
intersect(B, C)

# ¿Existe al menos un elemento de $A$ igual a un elemento en $B$?
intersect(A, B)

## Notación de conjuntos

union(A, C)
intersect(A, C)

## En la práctica

# Definamos un vector *moneda* con los posibles eventos y simulemos una sola tirada de esa moneda.
set.seed(1)
moneda <- c("cara", "sello")
sample(moneda, 1)

# Repitamos 5 veces este proceso
tirar_moneda <- sample(moneda, 5, replace = TRUE)
table(tirar_moneda)

# Repitamos 5.000 veces este proceso
tirar_moneda <- sample(moneda, 5000, replace = TRUE)
table(tirar_moneda)

## Variable aleatorias discretas
resultados <- replicate(expr = sum(sample(moneda, 3, replace = TRUE) == "cara"), n = 10000)
table(resultados)/10000

## Medidas de tendencia central

# Esperanza: Valor promedio/medio (ponderado).
mean(c(1,2,3,3,4,5,6,7,8,9,10,11,12))

# Mediana: Valor que divide las observaciones en dos grupos de igual tamaño.
median(c(1,2,3,3,4,5,6,7,8,9,10,11,12))

# Moda: Valor con la mayor probabilidad de ocurrencia (o mayor valor de la función de densidad)
table(c(1,2,3,3,4,5,6,7,8,9,10,11,12))

## Esperanza/Valor Eesperado

mean(resultados)

## Interpretaciónen la práctica

# Podemos decir que mu = 1.5 al tirar tres monedas es lo mismo que esperar obtener 15 caras al repetir este proceso 10 veces.
resultado_final <- vector()
for (i in 1:10000){
  resultado_por_proceso <- vector()
  for (j in 1:10){
    resultado_por_proceso[j] <- sum(sample(moneda, 3, replace = TRUE) == "cara")
  }
  resultado_final[i] <- sum(resultado_por_proceso)
}
hist(resultado_final)

## Varianza

var(resultados)

## Algo interesante...

dbinom(x = 3, size = 3, prob = 0.5) #pmf
pbinom(q = 3, size = 3, prob = 0.5) #cdf

## Área bajo la curva = 1

f_ejemplo <- function(x){12*(x^2)*(1-x)}
integrate(f_ejemplo, 0, 1)

## Probabilidad = porción del área

integrate(f_ejemplo, 0.25, 0.5)

## Probabilidad de un x = 0

integrate(f_ejemplo, 0.4, 0.4)

## Valor esperado

f_ejemplo_e <- function(x){x*(12*(x^2)*(1-x))}
integrate(f_ejemplo_e, 0, 1)

## Varianza

f_ejemplo_var <- function(x){x^2*(12*(x^2)*(1-x))}
integrate(f_ejemplo_var, 0, 1)[[1]] - (integrate(f_ejemplo_e, 0, 1))[[1]]^2

## Distribución normal

# Calcular probabilidades

# ¿Cuál es la probabilidad de que un estudiante tenga menos de 75 puntos?

pnorm(75, mean = 80, sd = 5)

# ¿Cuál es la probabilidad de que un estudiante tenga más de 85 puntos?

1 - pnorm(85, mean = 80, sd = 5)
pnorm(85, mean = 80, sd = 5, lower.tail = FALSE)

## N de sigma de la media

# Un/a estudiante X tuvo 85 puntos en la prueba. ¿A cuántas desviaciones estándar está desde la media?

(85-80)/5

# Un/a estudiante Y tuvo 77.5 puntos en la prueba. ¿A cuántas desviaciones está desde la media?

(77.5-80)/5

## Z-score - quizás recuerdan esto

pnorm(q = 2, mean = 0, sd = 1)

## Distribución normal estándar

dnorm(0, mean = 0, sd = 1)
pnorm(0, mean = 0, sd = 1)

pnorm(1, mean = 0, sd = 1) - pnorm(-1, mean = 0, sd = 1)
pnorm(2, mean = 0, sd = 1) - pnorm(-2, mean = 0, sd = 1)
pnorm(3, mean = 0, sd = 1) - pnorm(-3, mean = 0, sd = 1)

## Censo

censo <- read.csv("../datos/muestra_censo_2017.csv")
str(censo)

## Sabemos mu y sigma

mean(censo$edad)
sd(censo$edad)

## Muestras

# Una muestra de 100 observaciones

mean(sample(censo$edad, 100))

# Una segunda muestra de 100 observaciones

mean(sample(censo$edad, 100))

# Una tercera muestra de 100 observaciones

mean(sample(censo$edad, 100))

## Aplica también a proporciones

round(table(censo$p_originario)/10000, 3)*100

## Relaciones entre variables

str(mtcars)
cov(mtcars$hp, mtcars$mpg)
cor(mtcars$hp, mtcars$mpg)