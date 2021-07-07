# Cargar paquetes
library(readr)
library(ggplot2)

# Datos
(datos_mundo <- read_csv("../datos/datos_mundo2007.csv"))

# Gráficos con una variable

## Histograma | Gráfico base (datos)
ggplot(datos_mundo)

## Histograma | Agregar capa (`aes`)
ggplot(datos_mundo, aes(x = gdpPercap))

## Histograma | Agregar capa (`geom`)
ggplot(datos_mundo, aes(x = gdpPercap)) +
  geom_histogram()

## Histograma | cambiar algunos argumentos
ggplot(datos_mundo, aes(x = gdpPercap)) +
  geom_histogram(bins = 100, fill = "blue")

ggplot(datos_mundo, aes(x = gdpPercap)) +
  geom_histogram(binwidth = 5000, col = "blue")

## Histograma

ggplot(datos_mundo, aes(x = gdpPercap)) +
  geom_histogram(binwidth = 1000, col = "white")

ggplot(datos_mundo, aes(x = gdpPercap)) +
  geom_histogram(binwidth = 5000, col = "white")

ggplot(datos_mundo, aes(x = gdpPercap)) +
  geom_histogram(binwidth = 10000, col = "white")

ggplot(datos_mundo, aes(x = gdpPercap)) +
  geom_histogram(binwidth = 15000, col = "white")

# Gráficos con dos variables

## Una variable categórica y una numérica | Gráfico base
ggplot(datos_mundo, aes(x = continente, y = gdpPercap))

## Una variable categórica y una numérica | capa de puntos (`geom_point`)
ggplot(datos_mundo, aes(x = continente, y = gdpPercap)) +
  geom_point()

## Boxplot
ggplot(datos_mundo, aes(x = continente, y = gdpPercap)) +
  geom_boxplot()

## Juntar más de una capa (`geom`)
ggplot(datos_mundo, aes(x = continente, y = gdpPercap)) +
  geom_boxplot() +
  geom_point()

## Invertir los ejes
ggplot(datos_mundo, aes(x = continente, y = gdpPercap)) +
  geom_boxplot() +
  geom_point() +
  coord_flip()

## Dos variables numéricas | Gráfico de dispersión 
ggplot(datos_mundo, aes(x = gdpPercap, y = ExpVida)) +
  geom_point()

ggplot(datos_mundo, aes(x = gdpPercap, y = ExpVida)) +
  geom_point(col = "red")

## Gráfico de dispersión | Agregar información sobre continente
ggplot(datos_mundo, aes(x = gdpPercap, y = ExpVida)) +
  geom_point(aes(col = continente))

## Gráfico de dispersión | Agregar linea de tendencia
ggplot(datos_mundo, aes(x = gdpPercap, y = ExpVida)) +
  geom_point() +
  geom_smooth()

## Gráfico de dispersión | Agregar linea de tendencia
ggplot(datos_mundo, 
       aes(x = gdpPercap, y = ExpVida)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm")

ggplot(datos_mundo, 
       aes(x = gdpPercap, y = ExpVida)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm", 
              formula = y ~ log(x))

## GRÁFICO "RE CONSTRUÍDO" DE LA PPT

datos_graf <- data.frame(
  educ = c("Finish no school", "Finish 1 year", "Finish 3 years", "Graduate, 2.0 GPA", "Graduate, 3.0 GPA", "Graduate, 3.75 GPA"),
  inc = c(480, 520, 650, 810, 940, 1070))

ggplot(datos_graf, aes(x = reorder(educ, -inc), y = inc, label = inc)) +
  geom_col(width = 0.5, fill = "dark blue") +
  geom_text(nudge_y = 40, size = 3) +
  coord_flip() +
  labs(x = NULL, y = NULL) +
  scale_y_continuous(n.breaks = 9) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank()) +
  labs(title = "Discounted Expected Lifetime Earnings, VN(t')",
       subtitle = "(Income in thousands)")
