# leectura de datos

library(readr)
library(lubridate)
library(tidyverse)


Hurtos_ANT <- read_csv("base_data/Reporte_Hurto_de_motocicletas_y_automotores_en_el_Departamento_de_Antioquia_-_Policía_Nacional_20260930.csv"
)


# Procesar las fechas y agrupar por mes

#----serie general Antioquia----

df_mensual_antioqua <- Hurtos_ANT |> 
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
anio_inicio <- as.numeric(format(min(df_mensual_antioqua$fecha), "%Y"))
mes_inicio  <- as.numeric(format(min(df_mensual_antioqua$fecha), "%m"))


## serie ts

serie_ts_ant <- ts(
  df_mensual_antioqua$valor,
  start = c(anio_inicio, mes_inicio),
  frequency = 12
)



serie_ant <- leer_serie(
  x = serie_ts_ant,
  fuente = "datos abiertos col",
  unidad = "robos reportados de vehiculos"
)


graficar_serie(serie_ant, "Robos registrado en Antioquia ")

#----- Serie medellin----
df_mensual_med <- Hurtos_ANT |> 
  rename(T_hurto = `TIPO DE HURTO`) |> 
  filter(T_hurto == "ARTÍCULO 239. HURTO AUTOMOTORES" & MUNICIPIO == "Medellín (CT)") |> 
  mutate(
    fecha_dt = dmy(`FECHA HECHO`), 
    fecha = floor_date(fecha_dt, unit = "month")
  ) |> 
  group_by(fecha) |> 
  summarise(valor = sum(CANTIDAD, na.rm = TRUE), .groups = "drop") |> 
  arrange(fecha)

anio_inicio <- as.numeric(format(min(df_mensual_med$fecha), "%Y"))
mes_inicio  <- as.numeric(format(min(df_mensual_med$fecha), "%m"))


# serie ts

serie_ts_med <- ts(
  df_mensual_med$valor,
  start = c(anio_inicio, mes_inicio),
  frequency = 12
)



serie_med <- leer_serie(
  x = serie_ts_med,
  fuente = "datos abiertos col",
  unidad = "robos reportados de vehiculos"
)

graficar_serie(serie_med, "Robos registrado en Medellin ")


#----serie Rionegro----

df_mensual_Rio <- Hurtos_ANT |> 
  rename(T_hurto = `TIPO DE HURTO`) |> 
  filter(T_hurto == "ARTÍCULO 239. HURTO AUTOMOTORES" & MUNICIPIO == "Rionegro") |> 
  mutate(
    fecha_dt = dmy(`FECHA HECHO`), 
    fecha = floor_date(fecha_dt, unit = "month")
  ) |> 
  group_by(fecha) |> 
  summarise(valor = sum(CANTIDAD, na.rm = TRUE), .groups = "drop") |> 
  arrange(fecha)


anio_inicio <- as.numeric(format(min(df_mensual_Rio$fecha), "%Y"))
mes_inicio  <- as.numeric(format(min(df_mensual_Rio$fecha), "%m"))


# serie ts

serie_ts_Rio <- ts(
  df_mensual_Rio$valor,
  start = c(anio_inicio, mes_inicio),
  frequency = 12
)



serie_Rio <- leer_serie(
  x = serie_ts_Rio,
  fuente = "datos abiertos col",
  unidad = "robos reportados de vehiculos"
)


graficar_serie(serie_Rio, "Robos registrado en Rionegro ")





# serie envigado

df_mensual_Envi <- Hurtos_ANT |> 
  rename(T_hurto = `TIPO DE HURTO`) |> 
  filter(T_hurto == "ARTÍCULO 239. HURTO AUTOMOTORES" & MUNICIPIO == "Envigado") |> 
  mutate(
    fecha_dt = dmy(`FECHA HECHO`), 
    fecha = floor_date(fecha_dt, unit = "month")
  ) |> 
  group_by(fecha) |> 
  summarise(valor = sum(CANTIDAD, na.rm = TRUE), .groups = "drop") |> 
  arrange(fecha)

anio_inicio <- as.numeric(format(min(df_mensual_Envi$fecha), "%Y"))
mes_inicio  <- as.numeric(format(min(df_mensual_Envi$fecha), "%m"))


# serie ts

serie_ts_Envi <- ts(
  df_mensual_Envi$valor,
  start = c(anio_inicio, mes_inicio),
  frequency = 12
)



serie_Envi <- leer_serie(
  x = serie_ts_Envi,
  fuente = "datos abiertos col",
  unidad = "robos reportados de vehiculos"
)



graficar_serie(serie_Envi, "Robos registrado en Envigado ")


remove(Hurtos_ANT, df_mensual_Rio, df_mensual_med, df_mensual_Envi, df_mensual_antioqua, 
       anio_inicio, mes_inicio, serie_ts_ant, serie_ts_Rio, serie_ts_med, serie_ts_Envi)





