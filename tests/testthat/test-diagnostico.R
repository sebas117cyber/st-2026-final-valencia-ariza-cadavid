# Pruebas de R/04-diagnostico.R
source(here::here("R", "util-evaluacion.R"), encoding = "UTF-8")
source(here::here("R", "04-diagnostico.R"), encoding = "UTF-8")

test_that("jarque_bera() del equipo coincide con tseries", {
  set.seed(2)
  e <- rnorm(150)
  expect_equal(jarque_bera(e)$estadistico,
               round(unname(tseries::jarque.bera.test(e)$statistic), 4))
})

test_that("Ljung-Box resta K grados de libertad y McLeod-Li no", {
  set.seed(3)
  d <- diagnostico_residuos(rnorm(120), K = 2, rezagos = 24)
  expect_equal(d$gl[d$prueba == "Ljung-Box"], 22)
  expect_equal(d$gl[d$prueba == "McLeod-Li"], 24)
  expect_true(all(d$valor_p >= 0 & d$valor_p <= 1))
})

test_that("las pruebas de raíz unitaria detectan un paseo aleatorio", {
  set.seed(4)
  x <- cumsum(rnorm(200))
  r <- pruebas_raiz_unitaria(x)
  expect_false(r$rechaza_h0[r$prueba == "ADF"])
  expect_true(r$rechaza_h0[r$prueba == "KPSS"])
})
