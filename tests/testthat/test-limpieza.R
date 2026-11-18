# Pruebas de R/02-limpieza.R
source(here::here("R", "02-limpieza.R"), encoding = "UTF-8")

registros_prueba <- tibble::tibble(
  municipio     = c("Medellín (CT)", "Medellín (CT)", "Medellín (CT)", "Bello", "Medellín (CT)"),
  tipo_de_hurto = c(rep("ARTÍCULO 239. HURTO AUTOMOTORES", 4), "ARTÍCULO 239. HURTO MOTOCICLETAS"),
  fecha_hecho   = as.Date(c("2011-01-05", "2011-01-20", "2011-03-02", "2011-02-10", "2011-02-11")),
  cantidad      = c(1L, 1L, 1L, 1L, 1L)
)

test_that("cuenta por mes del hecho y rellena con cero los meses sin registros", {
  s <- construir_serie_mensual(registros_prueba, desde = "2011-01", hasta = "2011-04")
  expect_s3_class(s, "tbl_ts")
  expect_equal(s$hurtos, c(2L, 0L, 1L, 0L))
})

test_that("filtra municipio y tipo de hurto", {
  s <- construir_serie_mensual(registros_prueba, municipio = "Bello",
                               desde = "2011-01", hasta = "2011-03")
  expect_equal(s$hurtos, c(0L, 1L, 0L))
})

test_that("guardar y leer la serie la devuelve igual", {
  s    <- construir_serie_mensual(registros_prueba, desde = "2011-01", hasta = "2011-04")
  ruta <- tempfile(fileext = ".csv")
  guardar_serie(s, ruta)
  expect_equal(leer_serie_procesada(ruta)$hurtos, s$hurtos)
})
