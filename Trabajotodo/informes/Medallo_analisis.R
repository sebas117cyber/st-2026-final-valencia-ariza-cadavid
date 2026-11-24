

library(readr)
library(lubridate)
library(tidyverse)
library(plotly)
library(forecast)
library(tseries)
library(TSA)
library(lmtest)
library(ggplot2)
library(patchwork)
library(here)


Hurtos_ANT <- read_csv("Trabajotodo/base_data/Reporte_Hurto_de_motocicletas_y_automotores_en_el_Departamento_de_Antioquia_-_Policía_Nacional_20260930.csv"
)


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

remove(Hurtos_ANT, df_mensual_med, anio_inicio, mes_inicio)

source("Trabajotodo/R/00-lectura.R")

serie_med <- leer_serie(
  x = serie_ts_med,
  fuente = "datos abiertos col",
  unidad = "robos reportados de vehiculos"
)


G_originalM <- graficar_serie(serie_med, "Reporte de Robos de vehiculos en medellin 2010-2026", 
               color = "#00688B", mostrar_puntos = FALSE)


Cor_originalM <- correlograma(serie_med, m = 24)


boxcox_medallo <- evaluar_boxcox(serie_ts_med)

ST_transMedallo <- boxcox_medallo$serie_transformada

trans_medallo <- leer_serie(ST_transMedallo, 
                            fuente = "datos abiertos col",
                            unidad = "robos reportados de vehiculos")

G_transM <- graficar_serie(trans_medallo, "Reporte de Robos de vehiculos en medellin 2010-2026")


Cor_transM <- correlograma(trans_medallo, m = 24)

#----Diagnostico medallo ----

# 2. Identificar diferenciación (d) y patrones de ACF/PACF
res_id <- identificar_modelo(trans_medallo, rezagos = 24)
res_id$tabla_df
res_id$tabla_corr

# 3. Seleccionar candidatos automáticamente según la ACF y PACF
res_comp <- seleccionar_candidatos(
  y = trans_medallo,
  d = res_id$d,
  graficar = TRUE
)
res_comp$tabla
