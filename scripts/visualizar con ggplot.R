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
    

# boxplot
ggplot(data = datos) + # Carga datos que se usarán en el grafico
  geom_boxplot(aes(x=precio)) 


# Jugando con edades. Simulando
edades <- round(rnorm(n = 50, mean = 18, sd = 2))

edades <- c(edades, 95, 120, 1) # incluye valores outliers atípicos

edades


media <- round(mean(edades))
media



# Visualizando boxplot con ggplot 
# Ahora no usanmopd un data.frame sino simplemennte
# un arreglo de la variabl edades
ggplot() + # Carga datos que se usarán en el grafico
  geom_boxplot(aes(x=edades))


# Modificar los valores atipicos

q1 <- quantile(edades, 0.25)
q3 <- quantile(edades, 0.75)
RI<- q3 - q1
RI
l_inferior <- q1 - 1.5 * RI
l_superior <- q3 + 1.5 * RI

print ("Los Cuartiels, RI y limites")
RI
l_inferior
l_superior


# cuáles valores son los atípicos?
# qué hacemos con esos atípicos?
# limpiarlos

atipicos <- edades[edades < l_inferior | edades > l_superior]
atipicos

# Ponerles la media
edades_limpias <- ifelse(
  edades < l_inferior | edades > l_superior,
  media, # Cambiar los valores atípicos a la media
  edades
)

# un arreglo de la variabl edades
ggplot() + # Carga datos que se usarán en el grafico
  geom_boxplot(aes(x=edades_limpias))


# función que quita los datos atípicos
# y les pone la media aritmética de los mismos
f_limpiar_atipicos <- function(datos) {
  # Ponerles la media
  media <- round(mean(edades))
  media
  
  q1 <- quantile(datos, 0.25)
  q3 <- quantile(datos, 0.75)
  RI<- q3 - q1
  RI
  l_inferior <- q1 - 1.5 * RI
  l_superior <- q3 + 1.5 * RI
  
  RI
  l_inferior
  l_superior
  
  datos_limpios <- ifelse(
    datos < l_inferior | edades > l_superior,
    media, # Cambiar los valores atípicos a la media
    datos
  )
  
  return(datos_limpios)
}
 
 
