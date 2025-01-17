# Construir un modelo de regression polinomial de tercer nivel
# para compararlo con otros modelos

# cargar librerias
library(readr)
library(ggplot2)

# cargar las funciones
source("https://raw.githubusercontent.com/rpizarrog/Ciencia-de-los-Datos-Descriptivo-Predictivo/refs/heads/main/scripts/funciones_para_regresion.R")

# Cargar los datos
datos_entrenamiento <- read.csv("https://raw.githubusercontent.com/rpizarrog/Ciencia-de-los-Datos-Descriptivo-Predictivo/refs/heads/main/datos/train_data.csv")
datos_validacion <- read.csv("https://raw.githubusercontent.com/rpizarrog/Ciencia-de-los-Datos-Descriptivo-Predictivo/refs/heads/main/datos/test_data.csv")

str(datos_entrenamiento)
str(datos_validacion)


# crear modelo polinomico de tercer nivel
nivel <- 3
modelo_poly3 <- lm(data = datos_entrenamiento, formula = peso ~ poly(estatura, nivel, raw = TRUE))
summary(modelo_poly3)

# Visualizar curva
f_polinomial_curva(datos_entrenamiento, nivel)

# Hacer predicciones con el modelo
predicciones <- predict(modelo_poly3, datos_validacion)
predicciones

datos_comparar <- data.frame(reales = datos_validacion$peso, predicciones_poly3 = predicciones) 
datos_comparar

# Cálculo manual del RMSE
rmse <- sqrt(mean((datos_comparar$reales - datos_comparar$predicciones_poly3)^2))
# Imprimir resultado
paste ("RMSE:", rmse)



print ("comparado con otros modelos, ¿cuál es más eficiente conforme al estadístico RMSE ?")

