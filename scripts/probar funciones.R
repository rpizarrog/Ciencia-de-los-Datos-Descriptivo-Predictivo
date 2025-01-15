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
  
  hist(datos, 
       main =titulo,
       sub = paste("Me", round(media, 2), "; Mediana", round(mediana, 2),
                   "; ds", round(ds, 2), "; Max", round(maximo, 2), "; Min", round(minimo, 2),
                   ";Cuartiles", round(q1, 2), ", ", round(q2, 2), ", ", round(q3, 2)),
       xlab = "Xs",
       ylab = "Frecuencia")
}
