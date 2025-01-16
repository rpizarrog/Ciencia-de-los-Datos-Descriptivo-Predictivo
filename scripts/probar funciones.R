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

# Función para generar un color aleatorio
f_color_aleatorio <- function() {
  grDevices::rgb(runif(1), runif(1), runif(1))
}


# Función que devuelve los estadísticos más populares
f_estadisticos <- function(datos) {
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
  
  return(list(
    n = n,
    clases = clases,
    media = media,
    mediana = mediana,
    minimo = minimo,
    maximo = maximo,
    q1 = q1,
    q2 = q2,
    q3 = q3,
    RI = RI,
    ds = ds
  ))
}



# Genera un histograma personalizado con titulo de
# un conjunto de datos numéricos
f_histograma <- function (datos, titulo) {
  # Estadisticos 
  estadisticos <- f_estadisticos(datos)
  media <- estadisticos$media
  mediana <- estadisticos$mediana
  maximo <- estadisticos$maximo
  minimo <- estadisticos$minimo
  q1 <- estadisticos$q1
  q2 <- estadisticos$q2
  q3 <- estadisticos$q3
  
  RI <- estadisticos$RI
  
  ds <- estadisticos$ds
  
  n <- estadisticos$n # Número de observaciones
  clases <- estadisticos$clases # Número de clases según Sturges
  
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
  
  # Estadisticos 
  estadisticos <- f_estadisticos(datos$columna)
  media <- estadisticos$media
  mediana <- estadisticos$mediana
  maximo <- estadisticos$maximo
  minimo <- estadisticos$minimo
  q1 <- estadisticos$q1
  q2 <- estadisticos$q2
  q3 <- estadisticos$q3
  
  RI <- estadisticos$RI
  
  ds <- estadisticos$ds
  
  n <- estadisticos$n # Número de observaciones
  clases <- estadisticos$clases # Número de clases según Sturges
  
  # Crear el histograma con ggplot2
  ggplot(datos, aes(x = columna)) +
    geom_histogram(aes(y = after_stat(density)), bins = clases, fill = "skyblue", color = "black", alpha = 0.7) + # Histograma con densidad
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

# Diagrama de caja con boxplot() horizontal
f_diagcaja <- function(datos, titulo) {
  # Crear el diagrama de caja con título y subtítulo
  # Estadisticos 
  estadisticos <- f_estadisticos(datos)
  media <- estadisticos$media
  mediana <- estadisticos$mediana
  maximo <- estadisticos$maximo
  minimo <- estadisticos$minimo
  q1 <- estadisticos$q1
  q2 <- estadisticos$q2
  q3 <- estadisticos$q3
  
  RI <- estadisticos$RI
  
  ds <- estadisticos$ds
  
  n <- estadisticos$n # Número de observaciones
  clases <- estadisticos$clases # Número de clases según Sturges
  
  subtitulo = paste("Me", round(media, 2), "; Mediana", round(mediana, 2),
                    "; ds", round(ds, 2), "; Max", round(maximo, 2), "; Min", round(minimo, 2),
                    "; Qs", round(q1, 2), ", ", round(q2, 2), ", ", round(q3, 2),
                    "; RI", round(RI, 2))
  boxplot(datos,
          main = paste("Distribucion de ", titulo),
            sub = subtitulo,
          xlab = "X's", # Etiqueta del eje X
          ylab = "Valores", # Etiqueta del eje Y
          border = "black",horizontal = TRUE) # Color del borde
  
  # Agregar línea horizontal para la media
  abline(v = mean(datos), col = "red", lwd = 2, lty = 2)
}

# Diagrama de caja con boxplot() horizontal
f_diagcaja_ggplot <- function(valores, titulo) {
  # Crear el diagrama de caja ggplot con título y subtítulo
  # Estadísticos 
  # Calcular el binwidth para 10 cortes
  datos <- data.frame(x = valores)
  
  estadisticos <- f_estadisticos(datos$x)
  media <- estadisticos$media
  mediana <- estadisticos$mediana
  maximo <- estadisticos$maximo
  minimo <- estadisticos$minimo
  q1 <- estadisticos$q1
  q2 <- estadisticos$q2
  q3 <- estadisticos$q3
  
  RI <- estadisticos$RI
  
  ds <- estadisticos$ds
  
  n <- estadisticos$n # Número de observaciones
  clases <- estadisticos$clases # Número de clases según Sturges
  
  subtitulo = paste("Me", round(media, 2), "; Mediana", round(mediana, 2),
                    "; ds", round(ds, 2), "; Max", round(maximo, 2), "; Min", round(minimo, 2),
                    "; Qs", round(q1, 2), ", ", round(q2, 2), ", ", round(q3, 2),
                    "; RI", round(RI, 2))
  ggplot(datos, aes(x = x)) +
    geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) + 
    geom_vline(aes(xintercept = media, color = "Media"), linetype = "dashed", linewidth = 1) + # Línea de la media
    
    labs(
      title = paste("Distribucion de ", titulo),
      y = "Valores",
      subtitle = subtitulo
    ) +
    theme_minimal()
}





# función para boxplot con ggplot de un valor numérico en función de un variable categórica
f_boxplotv1v2 <- function (v1, v2, titulo, modo, color=FALSE) {
  datos <- data.frame(v1=v1, v2=v2)
  
  if (color == TRUE) {
    color_aleatorio <- f_color_aleatorio()
  } else{
    color_aleatorio = "gray"
  }

  # Crear el boxplot del precio en función del tipo
  if (modo == 'H') {
    ggplot(datos, aes(x = v1, y = v2)) +
      geom_boxplot(fill = color_aleatorio, color = "black", alpha = 0.7) +
      labs(
        title = titulo,
        x = "valores",
        y = "grupos"
      ) +
      theme_minimal()
    
  } else {
    ggplot(datos, aes(x = v2, y = v1)) +
      geom_boxplot(fill = color_aleatorio, color = "black", alpha = 0.7) +
      labs(
        title = titulo,
        x = "grupo",
        y = "valores"
      ) +
      theme_minimal()
  }

}


# función que quita los datos atípicos
# y les pone la media aritmética de los mismos
f_limpiar_atipicos <- function(datos) {
  # Ponerles la media
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




