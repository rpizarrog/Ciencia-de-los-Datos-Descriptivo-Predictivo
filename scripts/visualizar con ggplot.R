# Comprender en ggplot en R
# Son funciones que dependen de la librería previamente instalada
# ggplot2
# Necesitan un congiunto de datos data.frame


library(ggplot2)
library(readr)
datos <- read.csv("https://raw.githubusercontent.com/rpizarrog/Ciencia-de-los-Datos-Descriptivo-Predictivo/refs/heads/main/datos/datos_aguacate_limpio.csv")

media <- mean(datos$precio)
ds <- sd(datos$precio)

ggplot(data = datos) + # Carga datos que se usarán en el grafico
  geom_histogram(aes(x=precio)) +
  geom_vline(xintercept = media, color = 'red') + # Linbea vertical
  geom_vline(xintercept = media-ds, color = 'blue') + # Linea vertical
  geom_vline(xintercept = media+ds, color = 'blue')  # Linea vertical
    
