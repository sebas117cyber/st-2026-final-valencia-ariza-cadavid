# 04-diagnostico.R -------------------------------------------------------------
# Pruebas de raíz unitaria (OE2) y diagnóstico de residuos (OE3).
# Usa urca y tseries solo donde fable/feasts no tienen la prueba, y la función
# jarque_bera() de R/util-evaluacion.R.
# ------------------------------------------------------------------------------

#' Pruebas ADF, PP y KPSS de una serie
#'
#' ADF con constante y rezagos elegidos por AIC (máximo 12) con urca; PP y KPSS
#' con feasts. ADF y PP tienen H0 de raíz unitaria; KPSS, de estacionariedad.
#'
#' @param x Vector numérico (sin NA).
#' @param nivel Nivel de significancia.
#' @return tibble con prueba, H0, estadístico, valor crítico (ADF) o valor p
#'   (PP y KPSS, acotado por feasts entre 0,01 y 0,1) y decisión.
pruebas_raiz_unitaria <- function(x, nivel = 0.05) {
  adf  <- urca::ur.df(x, type = "drift", lags = 12, selectlags = "AIC")
  tau  <- unname(adf@teststat[1, "tau2"])
  crit <- unname(adf@cval["tau2", "5pct"])
  pp   <- feasts::unitroot_pp(x)
  kpss <- feasts::unitroot_kpss(x)
  tibble::tibble(
    prueba      = c("ADF", "PP", "KPSS"),
    h0          = c("Raíz unitaria", "Raíz unitaria", "Estacionariedad"),
    estadistico = c(tau, unname(pp["pp_stat"]), unname(kpss["kpss_stat"])),
    valor_critico = c(crit, NA, NA),
    valor_p     = c(NA, unname(pp["pp_pvalue"]), unname(kpss["kpss_pvalue"])),
    rechaza_h0  = c(tau < crit, pp["pp_pvalue"] < nivel, kpss["kpss_pvalue"] < nivel)
  )
}

#' Fuerza estacional de la descomposición STL y número de diferencias estacionales
#'
#' @param serie tsibble mensual.
#' @param variable Expresión de la variable (por ejemplo log(hurtos)).
#' @return tibble con F_S y D sugerido (1 si F_S >= 0,64).
fuerza_estacional <- function(serie, variable) {
  f <- fabletools::features(serie, {{ variable }}, feasts::feat_stl)
  tibble::tibble(fuerza_estacional = f$seasonal_strength_year,
                 D_sugerido = as.integer(f$seasonal_strength_year >= 0.64))
}

#' Diagnóstico de los residuos de un modelo
#'
#' @param e Residuos (innovaciones) del modelo.
#' @param K Número de parámetros AR y MA (se restan a los grados de libertad de Ljung-Box).
#' @param rezagos Rezagos de las pruebas de portmanteau.
#' @return tibble con prueba, H0, estadístico, grados de libertad y valor p.
diagnostico_residuos <- function(e, K, rezagos = 24) {
  lb <- stats::Box.test(e, lag = rezagos, type = "Ljung-Box", fitdf = K)
  ml <- stats::Box.test(e^2, lag = rezagos, type = "Ljung-Box")
  sw <- stats::shapiro.test(e)
  jb <- jarque_bera(e)
  tibble::tibble(
    prueba      = c("Ljung-Box", "McLeod-Li", "Shapiro-Wilk", "Jarque-Bera"),
    h0          = c("Residuos incorrelacionados", "Residuos al cuadrado incorrelacionados",
                    "Normalidad", "Normalidad"),
    estadistico = c(unname(lb$statistic), unname(ml$statistic), unname(sw$statistic), jb$estadistico),
    gl          = c(rezagos - K, rezagos, NA, 2),
    valor_p     = c(lb$p.value, ml$p.value, sw$p.value, jb$p_value)
  )
}

#' Residuos estandarizados con valor absoluto mayor que un umbral
#'
#' @param ajuste mable de una columna.
#' @param umbral Corte en desviaciones estándar.
#' @return tibble con mes, residuo estandarizado y valor observado.
atipicos_residuos <- function(ajuste, umbral = 3) {
  fabletools::augment(ajuste) |>
    tibble::as_tibble() |>
    dplyr::mutate(z = .innov / stats::sd(.innov, na.rm = TRUE)) |>
    dplyr::filter(abs(z) > umbral) |>
    dplyr::select(mes, hurtos, z)
}
