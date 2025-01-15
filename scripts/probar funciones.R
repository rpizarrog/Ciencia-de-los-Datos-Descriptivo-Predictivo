# Probando funciones
# Las funciones en programación son 
# piezas de código reutilizables o 
# El objetivo de las funciones es hacer tareas
# repetitivas
# Las funciones pueden recibir valores y 
# Las funciones se utilizan para devolver valores

# Ejemplos
f_saludar <- function (){
  print ("Hola estoy en la FUNCION saludar()")
}

# función saludar que recibe un argumento y/o parámetro
f_saludar_arg <- function (nombre) {
  paste("Hola, como estas ", nombre)
}

# Función matemática que regresa un valor
f_sumar <- function(a, b) {
  suma <- a + b
  return (suma) # el return devuelve lo que esta en ()
}

# Esta función devuelve los estadísticos descriptivos
f_descriptivos <- function(numeros) {
  return (summary(numeros))
}

# Genera un histograma personalizado con titulo de
# un conjunto de datos numéricos
f_histograma <- function (datos, titulo) {
  # Estadisticos con funciones específicas
  media <- mean(datos)
  mediana <- median(datos)
  maximo <- max(datos)
  minimo <- min(datos)
  q1 <- quantile(datos, 0.25)
  q2 <- quantile(datos, 0.50)
  q3 <- quantile(datos, 0.75)
  
  RI <- q3 - q1
  
  # Visualizar los valores
  # La funcion paste() es como print() pero puedes 
  # concatenar poner varios valores en la misma instrucción, juntar elementos

  # Nos falta la desviación estándar
  ds <- sd(datos) # sd es la función en inglçs y ds es la variable que yo pouse en espaniol
  
  # Calcular el número de intervalos usando la fórmula de Sturges
  n <- length(datos) # Número de observaciones
  clases <- ceiling(1 + log2(n)) # Número de clases según Sturges
  
  
  hist(datos, breaks = clases,
       main =titulo,
       sub = paste("Me", round(media, 2), "; Mediana", round(mediana, 2),
                   "; ds", round(ds, 2), "; Max", round(maximo, 2), "; Min", round(minimo, 2),
                   ";Cuartiles", round(q1, 2), ", ", round(q2, 2), ", ", round(q3, 2)),
       xlab = "Xs",
       ylab = "Frecuencia")
}


f_histograma_ggplot <- function(valores, titulo) {
  
  # Calcular el binwidth para 10 cortes
  datos <- data.frame(columna = valores)
  
  # Estadisticos con funciones específicas
  media <- mean(datos$columna)
  mediana <- median(datos$columna)
  maximo <- max(datos$columna)
  minimo <- min(datos$columna)
  q1 <- quantile(datos$columna, 0.25)
  q2 <- quantile(datos$columna, 0.50)
  q3 <- quantile(datos$columna, 0.75)
  
  RI <- q3 - q1
  
  # Nos falta la desviación estándar
  ds <- sd(datos$columna) # sd es la función en inglçs y ds es la variable que yo pouse en espaniol
  
  # Calcular el número de intervalos usando la fórmula de Sturges
  n <- nrow(datos) # Número de observaciones
  clases <- ceiling(1 + log2(n)) # Número de clases según Sturges
  
  # Crear el histograma con ggplot2
  ggplot(datos, aes(x = columna)) +
    geom_histogram(aes(y = ..density..), bins = clases, fill = "skyblue", color = "black", alpha = 0.7) + # Histograma con densidad
    geom_density(color = "red", linewidth = 1) + # Línea de densidad
    geom_vline(aes(xintercept = media, color = "Media"), linetype = "dashed", linewidth = 1) + # Línea de la media
    geom_vline(aes(xintercept = media + ds, color = "Media + SD"), linetype = "dotted", linewidth = 1) + # Media + Desviación estándar
    geom_vline(aes(xintercept = media - ds, color = "Media - SD"), linetype = "dotted", linewidth = 1) + # Media - Desviación estándar
    scale_color_manual(
      values = c("Media" = "green", "Media + SD" = "blue", "Media - SD" = "blue"),
      name = "Leyenda",
      labels = c(
        paste("Media =", round(media, 2)),
        paste("Media + SD =", round(media + ds, 2)),
        paste("Media - SD =", round(media - ds, 2))
      )
    ) +
    labs(
      title = titulo,
      x = "Precio",
      y = "Frecuencia"
    ) +
    theme_minimal() # Tema limpio
}




