# 03-modelos.R -----------------------------------------------------------------
# Identificación y estimación de los SARIMA con intervenciones (OE2 y OE3).
# Todas las funciones reciben los datos como argumento; ninguna lee objetos
# globales. La serie es un tsibble mensual con columnas `mes` y `hurtos`.
# ------------------------------------------------------------------------------

#' Agrega variables de intervención a la serie
#'
#' @param serie tsibble mensual con columna `mes`.
#' @param ao Meses ("AAAA-MM") con atípico aditivo: indicador 1 solo en ese mes.
#' @param ls Meses ("AAAA-MM") con cambio de nivel: indicador 1 desde ese mes.
#' @return La serie con columnas ao_AAAAMM y ls_AAAAMM.
agregar_intervenciones <- function(serie, ao = character(), ls = character()) {
  for (m in ao) {
    serie[[paste0("ao_", gsub("-", "", m))]] <- as.numeric(serie$mes == tsibble::yearmonth(m))
  }
  for (m in ls) {
    serie[[paste0("ls_", gsub("-", "", m))]] <- as.numeric(serie$mes >= tsibble::yearmonth(m))
  }
  serie
}

#' Fórmula de fable para un SARIMA(p,d,q)(P,D,Q)12 sobre box_cox(hurtos, lambda)
#'
#' Sin constante: con d = 1 una constante sería una deriva determinística, que
#' la serie no muestra en el entrenamiento.
#'
#' @param orden Vector c(p, d, q, P, D, Q).
#' @param regresores Nombres de las columnas de intervención.
#' @param lambda Parámetro de Box-Cox (0 = logaritmo).
formula_sarima <- function(orden, regresores = character(), lambda = 0) {
  lado_izq <- if (lambda == 0) "log(hurtos)" else sprintf("box_cox(hurtos, %.10g)", lambda)
  terminos <- c("0", regresores,
                sprintf("pdq(%d, %d, %d)", orden[1], orden[2], orden[3]),
                sprintf("PDQ(%d, %d, %d, period = 12)", orden[4], orden[5], orden[6]))
  stats::as.formula(paste(lado_izq, "~", paste(terminos, collapse = " + ")))
}

#' Ajusta un SARIMA con fable
#'
#' @inheritParams formula_sarima
#' @param datos tsibble con `hurtos` y las columnas de `regresores`.
#' @return mable de una columna llamada `sarima`.
ajustar_sarima <- function(datos, orden, regresores = character(), lambda = 0) {
  # fable evalúa la fórmula en este entorno: box_cox() debe existir aquí aunque
  # fabletools no esté cargado.
  box_cox <- fabletools::box_cox
  formula <- formula_sarima(orden, regresores, lambda)
  environment(formula) <- environment()
  fabletools::model(datos, sarima = fable::ARIMA(formula))
}

#' Resume un ajuste: AICc, número de parámetros y raíces
#'
#' @param ajuste mable de ajustar_sarima().
#' @return tibble de una fila con aicc, K (parámetros AR y MA), el módulo mínimo
#'   de las raíces AR y MA (Inf si no hay) y el |t| mínimo de los coeficientes ARMA.
resumir_ajuste <- function(ajuste) {
  g <- fabletools::glance(ajuste)
  if (is.null(g$AICc) || length(g$AICc) == 0) {
    return(tibble::tibble(aicc = NA_real_, K = NA_integer_, raiz_ar_min = NA_real_,
                          raiz_ma_min = NA_real_, t_min = NA_real_))
  }
  coefs <- fabletools::tidy(ajuste)
  arma  <- coefs[grepl("^(ar|ma|sar|sma)[0-9]+$", coefs$term), ]
  modulo_min <- function(r) if (length(r) == 0) Inf else min(Mod(r))
  tibble::tibble(
    aicc        = g$AICc,
    K           = nrow(arma),
    raiz_ar_min = modulo_min(g$ar_roots[[1]]),
    raiz_ma_min = modulo_min(g$ma_roots[[1]]),
    t_min       = if (nrow(arma) == 0) NA_real_ else min(abs(arma$statistic))
  )
}

#' Rejilla de SARIMA candidatos con d y D fijos
#'
#' Todos los modelos comparten d, D y la muestra, de modo que sus AICc son
#' comparables.
#'
#' @inheritParams ajustar_sarima
#' @param d,D Órdenes de diferenciación, fijados antes con las pruebas de raíz unitaria.
#' @param p_max,q_max,P_max,Q_max Órdenes máximos explorados.
#' @return tibble con una fila por especificación.
rejilla_sarima <- function(datos, regresores = character(), d = 1, D = 0,
                           p_max = 2, q_max = 2, P_max = 1, Q_max = 1, lambda = 0) {
  rejilla <- tidyr::expand_grid(p = 0:p_max, q = 0:q_max, P = 0:P_max, Q = 0:Q_max)
  resumen <- purrr::pmap(rejilla, function(p, q, P, Q) {
    ajuste <- suppressWarnings(ajustar_sarima(datos, c(p, d, q, P, D, Q), regresores, lambda))
    resumir_ajuste(ajuste)
  })
  dplyr::bind_cols(rejilla, dplyr::bind_rows(resumen))
}

#' Regla de selección declarada antes de ajustar
#'
#' 1. Se descartan los modelos que no convergen o con una raíz AR o MA de módulo
#'    menor que `umbral_raiz` (no estacionarios o no invertibles).
#' 2. Entre los admisibles a menos de `tolerancia` unidades del menor AICc, se
#'    elige el de menos parámetros AR y MA; si hay empate, el de menor AICc.
#'
#' @param rejilla Salida de rejilla_sarima().
#' @return La rejilla con las columnas admisible, cerca_del_minimo y elegido.
elegir_sarima <- function(rejilla, tolerancia = 2, umbral_raiz = 1.01) {
  rejilla <- dplyr::mutate(
    rejilla,
    admisible = !is.na(aicc) & raiz_ar_min > umbral_raiz & raiz_ma_min > umbral_raiz
  )
  minimo <- min(rejilla$aicc[rejilla$admisible])
  rejilla <- dplyr::mutate(rejilla, cerca_del_minimo = admisible & aicc <= minimo + tolerancia)
  candidatos <- dplyr::arrange(dplyr::filter(rejilla, cerca_del_minimo), K, aicc)
  dplyr::mutate(rejilla, elegido = p == candidatos$p[1] & q == candidatos$q[1] &
                  P == candidatos$P[1] & Q == candidatos$Q[1])
}

#' Log-verosimilitud en la escala original para varios lambda
#'
#' Suma el jacobiano (lambda - 1) * sum(log y) a la log-verosimilitud del modelo
#' sobre la serie transformada, para que sea comparable entre transformaciones.
#'
#' @inheritParams ajustar_sarima
#' @param lambdas Valores de lambda a comparar.
comparar_lambda <- function(datos, lambdas, orden, regresores = character()) {
  purrr::map_dfr(lambdas, function(l) {
    ll_w <- fabletools::glance(ajustar_sarima(datos, orden, regresores, lambda = l))$log_lik
    tibble::tibble(lambda = l, loglik_transformada = ll_w,
                   loglik_original = ll_w + (l - 1) * sum(log(datos$hurtos)))
  })
}
