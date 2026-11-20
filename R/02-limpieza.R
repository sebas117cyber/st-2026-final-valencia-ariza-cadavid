# 02-limpieza.R ----------------------------------------------------------------
# Construye la serie mensual de hurtos de automóviles denunciados en Medellín a
# partir de los registros crudos. Lo que produce va a datos/procesados/ y nunca
# se edita a mano.
# ------------------------------------------------------------------------------

#' Lee un CSV de registros con nombres de columna normalizados
#'
#' Acepta tanto la exportación del portal (MUNICIPIO, FECHA HECHO, ...) como la
#' respuesta de la API (municipio, fecha_hecho, ...).
#'
#' @param ruta Ruta del CSV de registros.
#' @return tibble con municipio, tipo_de_hurto, fecha_hecho (Date) y cantidad.
leer_registros_hurto <- function(ruta) {
  registros <- readr::read_csv(ruta, col_types = readr::cols(.default = "c"),
                               locale = readr::locale(encoding = "UTF-8"))
  names(registros) <- gsub("\\s+", "_", tolower(names(registros)))
  registros |>
    dplyr::transmute(
      municipio,
      tipo_de_hurto,
      fecha_hecho = as.Date(fecha_hecho, format = "%d/%m/%Y"),
      cantidad    = as.integer(cantidad)
    )
}

#' Serie mensual de hurtos para un municipio y un tipo de hurto
#'
#' Cuenta los hurtos por mes de la fecha del hecho, rellena con cero los meses
#' sin registros y recorta al periodo pedido.
#'
#' @param registros Salida de leer_registros_hurto().
#' @param municipio Nombre exacto del municipio en la fuente.
#' @param tipo Texto que identifica el tipo de hurto ("AUTOMOTORES" o "MOTOCICLETAS").
#' @param desde,hasta Primer y último mes, en formato "AAAA-MM".
#' @return tsibble mensual con columnas mes (yearmonth) y hurtos.
construir_serie_mensual <- function(registros, municipio = "Medellín (CT)",
                                    tipo = "AUTOMOTORES",
                                    desde = "2011-01", hasta = "2025-12") {
  meses <- seq(as.Date(tsibble::yearmonth(desde)), as.Date(tsibble::yearmonth(hasta)),
               by = "month")
  conteos <- registros |>
    dplyr::filter(.data$municipio == .env$municipio,
                  grepl(tipo, tipo_de_hurto, fixed = TRUE)) |>
    dplyr::count(mes = tsibble::yearmonth(fecha_hecho), wt = cantidad, name = "hurtos")
  tibble::tibble(mes = tsibble::yearmonth(meses)) |>
    dplyr::left_join(conteos, by = "mes") |>
    dplyr::mutate(hurtos = dplyr::coalesce(hurtos, 0L)) |>
    tsibble::as_tsibble(index = mes)
}

#' Escribe la serie procesada en datos/procesados/
#'
#' @param serie tsibble de construir_serie_mensual().
#' @param ruta Ruta del CSV de salida.
guardar_serie <- function(serie, ruta = here::here("datos", "procesados",
                                                   "hurtos_autos_medellin_mensual.csv")) {
  dir.create(dirname(ruta), recursive = TRUE, showWarnings = FALSE)
  serie |>
    tibble::as_tibble() |>
    dplyr::mutate(mes = format(as.Date(mes), "%Y-%m")) |>
    readr::write_csv(ruta)
  invisible(ruta)
}

#' Lee la serie procesada como tsibble mensual
#'
#' @param ruta Ruta del CSV escrito por guardar_serie().
leer_serie_procesada <- function(ruta = here::here("datos", "procesados",
                                                   "hurtos_autos_medellin_mensual.csv")) {
  readr::read_csv(ruta, col_types = "ci") |>
    dplyr::mutate(mes = tsibble::yearmonth(mes)) |>
    tsibble::as_tsibble(index = mes)
}
