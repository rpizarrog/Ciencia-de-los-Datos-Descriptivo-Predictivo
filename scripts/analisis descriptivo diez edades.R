# Análisi descriptivo de diez edades
edades <- c(25, 30, 42, 56, 32, 28, 68, 34, 40, 26)
edades

# Los esadísticos comn summary()
summary(edades)


# Estadisticos con funciones específicas
media <- mean(edades)
mediana <- median(edades)
maximo <- max(edades)
minimo <- min(edades)
q1 <- quantile(edades, 0.25)
q2 <- quantile(edades, 0.50)
q3 <- quantile(edades, 0.75)

RI <- q3 - q1

# Visualizar los valores
# La funcion paste() es como print() pero puedes 
# concatenar poner varios valores en la misma instrucción, juntar elementos

paste("El valor mínimo es : ", minimo)
paste("El valor máximo es : ", maximo)
paste("La mediana es : ", mediana)
paste("El q1 es: ", q1)
paste("El q2 es: ", q2)
paste("El q3 es: ", q3)
paste("El Rango Intercuartilo es: ", RI)


# Nos falta la desviación estándar
varianza <- var(edades)
ds <- sd(edades)

paste("La varianza es: ", varianza)
paste("La desv. std es: ", ds)






