# Pruebas de R/03-modelos.R
source(here::here("R", "03-modelos.R"), encoding = "UTF-8")

serie_prueba <- tsibble::tsibble(
  mes = tsibble::yearmonth("2011-01") + 0:5, hurtos = c(10, 12, 9, 11, 13, 12), index = mes
)

test_that("las intervenciones marcan un mes (AO) o un tramo (LS)", {
  s <- agregar_intervenciones(serie_prueba, ao = "2011-03", ls = "2011-05")
  expect_equal(s$ao_201103, c(0, 0, 1, 0, 0, 0))
  expect_equal(s$ls_201105, c(0, 0, 0, 0, 1, 1))
})

test_that("la fórmula incluye transformación, regresores y órdenes", {
  f <- deparse1(formula_sarima(c(0, 1, 1, 0, 0, 1), "ao_201103"))
  expect_match(f, "log(hurtos)", fixed = TRUE)
  expect_match(f, "ao_201103", fixed = TRUE)
  expect_match(f, "pdq(0, 1, 1)", fixed = TRUE)
  expect_match(f, "PDQ(0, 0, 1, period = 12)", fixed = TRUE)
  expect_match(deparse1(formula_sarima(c(0, 1, 1, 0, 0, 0), lambda = 0.5)), "box_cox(hurtos, 0.5)", fixed = TRUE)
})

test_that("la regla descarta no invertibles y prefiere el modelo más simple cerca del mínimo", {
  rejilla <- tibble::tibble(
    p = c(1, 0, 0, 0), q = c(2, 1, 1, 2), P = 0, Q = c(1, 1, 0, 1),
    aicc = c(-64, -61.2, -59.3, -61.4), K = c(4, 2, 1, 3),
    raiz_ar_min = c(1.2, Inf, Inf, Inf), raiz_ma_min = c(1.0, 1.11, 1.4, 1.11), t_min = 2
  )
  r <- elegir_sarima(rejilla)
  expect_false(r$admisible[1])                     # raíz MA sobre el círculo unitario
  expect_false(r$cerca_del_minimo[3])              # a más de 2 unidades del mínimo admisible
  expect_equal(which(r$elegido), 2)                # K = 2 gana a K = 3
})

test_that("con lambda = 1 el jacobiano es cero", {
  set.seed(1)
  s <- tsibble::tsibble(mes = tsibble::yearmonth("2011-01") + 0:59,
                        hurtos = 50 + cumsum(rnorm(60)), index = mes)
  r <- comparar_lambda(s, lambdas = c(0, 1), orden = c(0, 1, 1, 0, 0, 0))
  expect_equal(r$loglik_original[r$lambda == 1], r$loglik_transformada[r$lambda == 1])
  expect_equal(r$loglik_original[r$lambda == 0],
               r$loglik_transformada[r$lambda == 0] - sum(log(s$hurtos)))
})
