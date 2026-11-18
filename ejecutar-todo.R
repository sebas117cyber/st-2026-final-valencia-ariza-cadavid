# ejecutar-todo.R --------------------------------------------------------------
# Script maestro: reconstruye desde los datos crudos todo lo que usa el informe.
# Ejecutar desde la raíz del proyecto, después de renv::restore().
#
#   Rscript ejecutar-todo.R              usa la copia versionada de los registros
#   Rscript ejecutar-todo.R --descargar  descarga antes una copia nueva y la compara
# ------------------------------------------------------------------------------

set.seed(20261118)
for (f in c("01-descarga.R", "02-limpieza.R")) {
  source(here::here("R", f), encoding = "UTF-8")
}

ruta_registros <- here::here("datos", "crudos", "hurtos_antioquia_registros_20260930.csv")

# 1. Descarga (opcional) ---------------------------------------------------------
if ("--descargar" %in% commandArgs(trailingOnly = TRUE)) {
  ruta_nueva <- descargar_registros_hurto()
  diferencias <- comparar_descargas(ruta_nueva, ruta_registros)
  message("Meses de Medellín que cambiaron frente a la copia versionada: ", nrow(diferencias))
}

# 2. Serie mensual de Medellín, enero de 2011 a diciembre de 2025 ---------------
serie <- construir_serie_mensual(leer_registros_hurto(ruta_registros))
stopifnot(nrow(serie) == 180, sum(serie$hurtos) == 17466)
guardar_serie(serie)

# 3. Pruebas de las funciones propias --------------------------------------------
testthat::test_dir(here::here("tests", "testthat"), stop_on_failure = TRUE)

message("Listo: ", nrow(serie), " meses, ", sum(serie$hurtos), " hurtos.")
