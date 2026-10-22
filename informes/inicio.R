# leectura de datos

library(readr)
library(lubridate)
library(tidyverse)

# Hurtos_ANT <- read_csv("base_data/Reporte_Hurto_de_motocicletas_y_automotores_en_el_Departamento_de_Antioquia_-_Policía_Nacional_20260930.csv"
#                        )
#---- Intento 1 -----
# Hurtos_ANT <- Hurtos_ANT |> 
#   rename(fechas = `FECHA HECHO`, valor = `CANTIDAD`, T_hurto = `TIPO DE HURTO`)
# 
# # base_motos <- Hurtos_ANT |> 
# #   filter(T_hurto == "ARTÍCULO 239. HURTO MOTOCICLETAS") |> 
# #   select(fechas, valor) |>  
# #   mutate(fechas = dmy(fechas)) |>  
# #   group_by(fechas = floor_date(fechas, unit = "month")) |>  
# #   summarise(valor = sum(valor, na.rm = TRUE))
# #   
# 
# 
# base_ts <- Hurtos_ANT |>  
#   filter(T_hurto == "ARTÍCULO 239. HURTO AUTOMOTORES") |>
#   select(fechas, valor) |>  
#   mutate(fechas = dmy(fechas)) |>  
#   group_by(fechas = floor_date(fechas, unit = "month")) |>  
#   summarise(valor = sum(valor, na.rm = TRUE))
#   
# fecha_inicio <- year(min(base_ts$fechas))
# mes_inicio <- month(min(base_ts$fechas))
# 
# serie_ts <- ts(
#   data = base_ts, 
#   start = c(fecha_inicio, mes_inicio), 
#   frequency = 12
# )
# 
# serie_ts1 <- serie_ts[, 1]
# 
# vector_st <- as.numeric(serie_ts1)
# 
# serie_hurto <- leer_serie(vector_st, "datos abiertos col", "robos reportados de vehiculos")
# 
# remove(serie_ts, serie_ts1, Hurtos_ANT, base_ts, fecha_inicio, tri_inicio)
# 
# grafico_ini <- Graficar_serie(serie_hurto , "Hurtos en antioquia 2010-2026")
# 
# correlograma(serie_hurto)
# ----- intento 2 ----

Hurtos_ANT <- read_csv("base_data/Reporte_Hurto_de_motocicletas_y_automotores_en_el_Departamento_de_Antioquia_-_Policía_Nacional_20260930.csv"
)


# Procesar las fechas y agrupar por mes
df_mensual <- Hurtos_ANT |> 
  rename(T_hurto = `TIPO DE HURTO`) |> 
  filter(T_hurto == "ARTÍCULO 239. HURTO AUTOMOTORES") |> 
  mutate(
    fecha_dt = dmy(`FECHA HECHO`), 
    fecha = floor_date(fecha_dt, unit = "month")
  ) |> 
  group_by(fecha) |> 
  summarise(valor = sum(CANTIDAD, na.rm = TRUE), .groups = "drop") |> 
  arrange(fecha)


# Obtener el año y mes inicial de los datos
anio_inicio <- as.numeric(format(min(df_mensual$fecha), "%Y"))
mes_inicio  <- as.numeric(format(min(df_mensual$fecha), "%m"))

# Crear la serie de tiempo (frecuencia 12 para datos mensuales)
serie_ts <- ts(
  df_mensual$valor,
  start = c(anio_inicio, mes_inicio),
  frequency = 12
)

# Pasar el objeto ts directamente a la función leer_serie
serie_final <- leer_serie(
  x = serie_ts,
  fuente = "datos abiertos col",
  unidad = "robos reportados de vehiculos"
)

remove(Hurtos_ANT, df_mensual, anio_inicio, mes_inicio, serie_ts)

graficar_serie(serie_final, "Robos Antioquia")



