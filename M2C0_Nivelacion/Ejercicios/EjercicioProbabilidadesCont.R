# Definir la función
f_ejer <- function(x){(1/(sqrt(2*pi)))*(exp(1)^(-(x^2)/(2)))}

#-----------#
# Preguntas #
#-----------#

# Reemplace donde vea "***" con lo que considera correcto

# ¿Es el área bajo la curva de la función (entre -7 y 7) igual a 1?
integrate(f_ejer, ***, ***)

# Calcule P(-7 < X < 0)
***(f_ejer, ***, 0)

# Calcule P(0 < X < 7)
integrate(f_ejer, 0, ***)

# ¿Cuál es el valor esperado de la función?
f_ejer_e <- function(x){(***)*(1/(sqrt(2*pi)))*(exp(1)^(-(x^2)/(2)))}
integrate(f_ejer_e, -7, 7)

# ¿Cuál es la varianza de la función?
f_ejer_var <- function(x){(***^2)*(1/(sqrt(2*pi)))*(exp(1)^(-(x^2)/(2)))}
integrate(f_ejer_var, -7, 7)

integrate(f_ejer_var, -7, 7)[[1]] - integrate(f_ejer_e, -7, 7)[[1]]^2

# Grafique la función
plot(seq(-7,7,0.1), f_ejer(seq(-7,7,0.1)))
