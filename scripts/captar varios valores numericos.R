# Captar varios valores
valores <- c()  # Inicializamos un vector vacío
for (i in 1:3) {
  valor <- as.numeric(readline(prompt = paste0("Introduce el valor ", i, ": ")))
  if (!is.na(valor)) {
    valores <- c(valores, valor)
  } else {
    cat("Entrada no válida, se ignorará.\n")
  }
}
cat("Los valores introducidos son:", valores, "\n")
