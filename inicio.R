# leectura de datos

library(readr)
library(tidyverse)

Hurtos_ANT <- read_csv("base_data/Reporte_Hurto_de_motocicletas_y_automotores_en_el_Departamento_de_Antioquia_-_Policía_Nacional_20260930.csv")

Hurtos_ANT <- Hurtos_ANT |> 
  rename(fechas = `FECHA HECHO`, valor = `CANTIDAD`)


base_ts <- Hurtos_ANT |> 
  rename(fechas = `FECHA HECHO`, valor = `CANTIDAD`) |> 
  select(fechas, valor) |> 
  mutate(fechas = dmy(fechas)) |> 
  group_by(fechas = floor_date(fechas, unit = "quarter")) |> 
  summarise(valor = sum(valor, na.rm = TRUE))
  
fecha_inicio <- year(min(base_ts$fechas))
tri_inicio <- quarter(min(base_ts$fechas))

serie_ts <- ts(
  data = base_ts, 
  start = c(fecha_inicio, tri_inicio), 
  frequency = 4
)

serie_ts1 <- serie_ts[, 1]


serie_hurto <- leer_serie(serie_ts1, "datos abiertos col", "robos reportados de vehiculos")

remove(serie_ts, serie_ts1, Hurtos_ANT, base_ts, fecha_inicio, tri_inicio)

grafico_ini <- Graficar_serie(serie_hurto , "Hurtos en antioquia 2010-2026")

correlograma(serie_hurto)
