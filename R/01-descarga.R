# 01-descarga.R ----------------------------------------------------------------
# Descarga los registros de hurto de motocicletas y automotores en Antioquia
# desde datos abiertos (Policía Nacional, conjunto Socrata 96zn-kpak).
#
# Cada fila es un hecho denunciado (columna `cantidad` = 1). La descarga guarda
# una copia fechada en datos/crudos/ porque la fuente revisa sus cifras; el
# pipeline usa por defecto la copia versionada en el repositorio
# (hurtos_antioquia_registros_20260930.csv) para que el resultado sea
# reproducible aunque la fuente cambie.
#
# Verificación (18 de noviembre de 2026): 144 040 registros en total,
# 28 403 de automotores y 18 824 de automotores en Medellín, iguales a la copia
# versionada del 30 de septiembre de 2026.
# ------------------------------------------------------------------------------

#' Descarga todos los registros del conjunto 96zn-kpak
#'
#' @param dir_salida Carpeta donde se guarda el CSV.
#' @param limite Máximo de filas que se piden a la API (el conjunto tiene ~144 000).
#' @return Ruta del archivo escrito (invisible).
descargar_registros_hurto <- function(dir_salida = here::here("datos", "crudos"),
                                      limite = 500000) {
  url <- paste0("https://www.datos.gov.co/resource/96zn-kpak.csv?$limit=",
                format(limite, scientific = FALSE))
  registros <- readr::read_csv(url, col_types = readr::cols(.default = "c"))
  if (nrow(registros) >= limite) {
    stop("La descarga llegó al límite de filas; aumente `limite`.")
  }
  dir.create(dir_salida, recursive = TRUE, showWarnings = FALSE)
  ruta <- file.path(dir_salida, sprintf("hurtos_antioquia_registros_%s.csv",
                                        format(Sys.Date(), "%Y%m%d")))
  readr::write_csv(registros, ruta)
  invisible(ruta)
}

#' Compara una descarga nueva con la copia versionada
#'
#' Cuenta los hurtos de automotores en Medellín por mes en ambos archivos y
#' devuelve los meses en que difieren (tabla vacía si coinciden).
#'
#' @param ruta_nueva,ruta_base Rutas de los dos CSV de registros.
comparar_descargas <- function(ruta_nueva, ruta_base) {
  contar <- function(ruta) {
    leer_registros_hurto(ruta) |>
      dplyr::filter(municipio == "Medellín (CT)",
                    grepl("AUTOMOTORES", tipo_de_hurto)) |>
      dplyr::count(mes = tsibble::yearmonth(fecha_hecho), wt = cantidad)
  }
  dplyr::full_join(contar(ruta_nueva), contar(ruta_base),
                   by = "mes", suffix = c("_nueva", "_base")) |>
    dplyr::filter(is.na(n_nueva) | is.na(n_base) | n_nueva != n_base)
}
