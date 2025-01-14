# Cargar datos de aguacate


# 1 Cargar librerías

# install.packages("readr")
library(readr)

print ("Ya cargaste librerias")
# 2 Cargar los datos

# Cargar datos de manera local

# read_csv() Es una función para cargar datos tipo csv
# read.csv() Es función alternativa pra cargar datos de url tipo texto

datos_aguacate <- read_csv("/cloud/project/datos/avocado.csv")

# Cargar desde una url de internet
datos_aguacate_url = read.csv("https://raw.githubusercontent.com/rpizarrog/Ciencia-de-los-Datos-Descriptivo-Predictivo/refs/heads/main/datos/avocado.csv")


# 3 Explorar preeliminar los datos. Análisis Descruiptivo
summary(datos_aguacate)

colnames(datos_aguacate) # Dame los nombres de variables o columnas


# Variables de interés
# Date . Fecha de registro de la observación cuando se recibe
# AveragePrice. El precio promedio por pieza unidad
# type Acuacate organico o convencional dependiendo del tipo de cultivo
# region Dónde se adquirió adquirió

# 3 Contexto de los datos
# Que preguntas de investigación o que quiero responder?

# ¿Cuáles son los estadísticos de la variable precio, media, moda, mediana y los cuartiles?. Descriptivo Texto
# ¿Cuál tipo de aguacate se consume mas Orgánico o el Convencional?. Frecuencias Descriptivo Tabla y visual
# ¿Hay relación del precio con respecto a la region. Descriptivo. Visual Barra apilada 
# ¿Cuál región come o compra mas?. Barra por la frecuencia


# 4 Preparar los datos
# Cargar librería dplyr toda vez que esté instalada
# install.packages(dplyr)
library(dplyr) # Sirve para utilizar funciones para extraer, filtras transformar ...

datos <- select(datos_aguacate, Date, AveragePrice, type, region)

colnames(datos) <- c("fecha", "precio", "tipo", "region") # Para cambiar nombres de variables


# Factorizar o categorizar los datos que son tipo char
datos$tipo <- as.factor(datos$tipo)
datos$region <- as.factor(datos$region)


# Se requiere trabajar con datos tipo date o fecha
# install.packages("lubridate") # PAra trabajar con datos tipo fecha
library(lubridate) # Se carga lubridate previamente instalado

datos$fecha <- date(datos$fecha)
summary(datos)

# Análisi Descriptivo

# ***********************************
# ¿Cuáles son los estadísticos de la variable precio, media, moda, mediana y los cuartiles?. Descriptivo Texto
summary(datos$precio)

# Visual
# Instalar ggplot2 para utilziar funciones de visualziacion de datos
# install.packages("ggplot2")
library(ggplot2)
# Calcular la media y la desviación estándar
media_precio <- mean(datos$precio)
ds_precio <- sd(datos$precio)



# Histograma de los precios
# Crear el histograma con ggplot2
# Crear el histograma con ggplot2
ggplot(datos, aes(x = precio)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black", alpha = 0.7) + # Histograma
  geom_vline(aes(xintercept = media_precio, color = "Media"), linetype = "dashed", size = 1) + # Línea de la media
  geom_vline(aes(xintercept = media_precio + ds_precio, color = "Media + SD"), linetype = "dotted", size = 1) + # Media + Desviación estándar
  geom_vline(aes(xintercept = media_precio - ds_precio, color = "Media - SD"), linetype = "dotted", size = 1) + # Media - Desviación estándar
  scale_color_manual(
    values = c("Media" = "red", "Media + SD" = "blue", "Media - SD" = "blue"),
    name = "Leyenda",
    labels = c(
      paste("Media =", round(media_precio, 2)),
      paste("Media + SD =", round(media_precio + ds_precio, 2)),
      paste("Media - SD =", round(media_precio - ds_precio, 2))
    )
  ) +
  labs(
    title = "Precio de aguacate",
    x = "Precio",
    y = "Frecuencia"
  ) +
  theme_minimal() # Tema limpio

# ***********************************



# Cual tipo de aguacate se consume mas Orgánico o el Convencional?. Frecuencias Descriptivo Tabla y visual
# ***********************************
# Se requiere una grafica de barra por el tipo de aguacate
table(datos$tipo) # Genera una tabla 

# Crear la gráfica de barras
ggplot(datos, aes(x = tipo, fill = tipo)) +
  geom_bar(alpha = 0.7, color = "black") + # Barras con color y contorno negro
  geom_text(
    stat = "count", 
    aes(label = ..count..), 
    vjust = -0.5, 
    size = 5
  ) + # Agregar etiquetas con los valores encima de las barras
  labs(
    title = "Cantidad de Aguacates por Tipo",
    x = "Tipo de Aguacate",
    y = "Cantidad"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right" # Ubicar la leyenda a la derecha
  )



# ***********************************
# Hay relación del precio con respecto a la region. Descriptivo. Visual Barra apilada 
# Cargar la librería dplyr
library(dplyr)

# Calcular el promedio de precio por región y ordenarlo de forma descendente
promedios <- datos %>%
  group_by(region) %>%
  summarise(promedio_precio = mean(precio, na.rm = TRUE)) %>%
  arrange(desc(promedio_precio))

# Mostrar resultados
print(promedios)


# Visualmente
# Cargar librerías necesarias
library(dplyr)
library(ggplot2)

# Calcular el promedio de precio por región
promedios <- datos %>%
  group_by(region) %>%
  summarise(promedio_precio = mean(precio, na.rm = TRUE)) %>%
  arrange(desc(promedio_precio)) # Ordenar de mayor a menor

# Separar top 5 y bottom 5
top_5 <- head(promedios, 5)       # Los 5 con precios más altos
bottom_5 <- tail(promedios, 5)    # Los 5 con precios más bajos

# Gráfico de barras para el top 5
ggplot(top_5, aes(x = reorder(region, promedio_precio), y = promedio_precio, fill = region)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Top 5 Regiones con Precios Medios Más Altos",
    x = "Región",
    y = "Precio Medio"
  ) +
  theme_minimal()

# Gráfico de barras para el bottom 5
ggplot(bottom_5, aes(x = reorder(region, promedio_precio), y = promedio_precio, fill = region)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Bottom 5 Regiones con Precios Medios Más Bajos",
    x = "Región",
    y = "Precio Medio"
  ) +
  theme_minimal()



# ***********************************


# ***********************************
# ¿Cuál región come o compra mas?. Barra por la frecuencia
tabla <- data.frame(table(datos$region))
tabla
# Cargar librerías necesarias
library(ggplot2)
library(dplyr)

# Calcular la frecuencia de observaciones por región
frecuencias <- datos %>%
  count(region) %>% # Contar cuántas observaciones hay por región
  rename(frecuencia = n) # Renombrar la columna de frecuencia

# Crear el gráfico de barras
ggplot(frecuencias, aes(x = reorder(region, -frecuencia), y = frecuencia, fill = region)) +
  geom_bar(stat = "identity", show.legend = FALSE) + # Gráfico de barras
  labs(
    title = "Frecuencia de Observaciones por Región",
    x = "Región",
    y = "Frecuencia"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotar etiquetas del eje x
  )


# ***********************************