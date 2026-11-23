
library(forecast)
library(tseries)
library(TSA)
library(lmtest)
library(ggplot2)


# nececitan que la `serie` sea un objeto ts


separar_datos <- function(serie, h, nombre_entrenamiento = "entrenamiento") {
  n_total <- length(serie)
  
  if (h >= n_total) {
    stop("El número de datos a separar (h) debe ser menor a la longitud total de la serie.")
  }
  
  # Extraer entrenamiento (inicio) y prueba (final)
  entrenamiento <- head(serie, n_total - h)
  prueba <- tail(serie, h)
  
  # Asignar el conjunto de entrenamiento al entorno global con el nombre indicado
  assign(nombre_entrenamiento, entrenamiento, envir = .GlobalEnv)
  
  cat("- Datos totales:", n_total, "\n")
  cat("- Datos en '", nombre_entrenamiento, "' (entrenamiento): ", length(entrenamiento), "\n", sep="")
  cat("- Datos separados para prueba (h):", h, "\n")
  
  # También retornamos ambos por si se quieren capturar en una lista
  return(invisible(list(entrenamiento = entrenamiento, prueba = prueba)))
}


evaluar_boxcox <- function(serie) {
  
  # Encontrar el lambda óptimo
  lambda_opt <- BoxCox.lambda(serie)
  
  # Aplicar la transformación
  serie_transformada <- BoxCox(serie, lambda_opt)
  
  cat("Evaluación de Box-Cox:\n")
  cat("Lambda óptimo estimado:", round(lambda_opt, 4), "\n")
  if (lambda_opt > 0.9 && lambda_opt < 1.1) {
    cat("Sugerencia: Lambda cercano a 1. No es estrictamente necesaria la transformación.\n")
  }
  
  return(list(lambda = lambda_opt, serie_transformada = serie_transformada))
}
