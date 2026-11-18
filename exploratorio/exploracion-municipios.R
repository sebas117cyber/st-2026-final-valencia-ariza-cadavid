# leectura de datos

library(readr)
library(lubridate)
library(tidyverse)
library(plotly)

source(here::here("R", "util-lectura.R"))
source(here::here("R", "util-graficos.R"))

Hurtos_ANT <- read_csv(here::here("datos", "crudos", "hurtos_antioquia_registros_20260930.csv"))


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

# Grafico
graficar_serie(serie_ant, "Robos registrados en Antioquia ")

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

g1 <- graficar_serie(serie_med, "Robos registrados en Medellin ")
g_co <- correlograma(serie_med)

library(forecast); library(ggplot2); library(patchwork)

# Tranformaciones


lambda <- BoxCox.lambda(serie_ts_med, method = "guerrero")
serie_trans <- BoxCox(serie_ts_med, lambda = lambda)

vector_ <- as.vector(serie_trans)
datos_trans <- data.frame(fecha = df_mensual_med$fecha, valor_trans = vector_)

g2 <- ggplot(datos_trans, aes(x = fecha, y = valor_trans)) +
  geom_line(color = "darkslategray", linewidth = 0.75) +
  labs(title = "Serie medellin Tranformada box-cox", x = "Fecha", y = "Valor") +
  theme_minimal()

# Antes vs. Despues de tranformar
g1 + g2


# 2. Tener media estacionaria:

d <- ndiffs(serie_trans)
w <- diff(serie_trans, differences = d)
vector_w <- as.vector(w)
datos_trans2 <- data.frame(fecha = df_mensual_med$fecha[2:199], valor_trans2 = vector_w)
g3 <- ggplot(datos_trans2, aes(x = fecha, y = valor_trans2)) +
  geom_line(color = "darkslategray", linewidth = 0.75) +
  labs(title = "Serie medellin diferenciada", x = "Fecha", y = "Valor") +
  theme_minimal()

# Paso a paso de ir aplicando las dos tranformaciones:
g1 + g2 + g3

par(mfrow = c(1,2))
acf(w, main = "ACF")
pacf(w, main = "PACF")

w_estacional <- diff(w, lag = 12)

par(mfrow = c(1,2))
acf(as.numeric(w), main = "ACF")
pacf(as.numeric(w), main = "PACF")




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


graficar_serie(serie_Rio, "Robos registrados en Rionegro ")





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


# serie bello

df_mensual_Bello <- Hurtos_ANT |> 
  rename(T_hurto = `TIPO DE HURTO`) |> 
  filter(T_hurto == "ARTÍCULO 239. HURTO AUTOMOTORES" & MUNICIPIO == "Bello") |> 
  mutate(
    fecha_dt = dmy(`FECHA HECHO`), 
    fecha = floor_date(fecha_dt, unit = "month")
  ) |> 
  group_by(fecha) |> 
  summarise(valor = sum(CANTIDAD, na.rm = TRUE), .groups = "drop") |> 
  arrange(fecha)

anio_inicio <- as.numeric(format(min(df_mensual_Bello$fecha), "%Y"))
mes_inicio  <- as.numeric(format(min(df_mensual_Bello$fecha), "%m"))


# serie ts

serie_ts_Bello <- ts(
  df_mensual_Bello$valor,
  start = c(anio_inicio, mes_inicio),
  frequency = 12
)



serie_Bello <- leer_serie(
  x = serie_ts_Bello,
  fuente = "datos abiertos col",
  unidad = "robos reportados de vehiculos"
)



graficar_serie(serie_Bello, "Robos registrado en Bello ")





#---- Valle de aburra----

df_mensual_Valle <- Hurtos_ANT |>
  rename(T_hurto = `TIPO DE HURTO`) |>
  filter(T_hurto == "ARTÍCULO 239. HURTO AUTOMOTORES",
         MUNICIPIO %in% c("Medellín (CT)", "Bello", "Itagui", "Envigado", "Sabaneta",
                          "La Estrella", "Caldas", "Copacabana", "Girardota", "Barbosa")) |>
  mutate(
    fecha_dt = dmy(`FECHA HECHO`),
    fecha = floor_date(fecha_dt, unit = "month")
  ) |>
  group_by(fecha) |>
  summarise(valor = sum(CANTIDAD, na.rm = TRUE), .groups = "drop") |>
  arrange(fecha)

anio_inicio <- as.numeric(format(min(df_mensual_Valle$fecha), "%Y"))
mes_inicio  <- as.numeric(format(min(df_mensual_Valle$fecha), "%m"))


# serie ts

serie_ts_Valle <- ts(
  df_mensual_Valle$valor,
  start = c(anio_inicio, mes_inicio),
  frequency = 12
)



serie_Valle <- leer_serie(
  x = serie_ts_Valle,
  fuente = "datos abiertos col",
  unidad = "robos reportados de vehiculos"
)



graficar_serie(serie_Valle, "Robos de autos registrados en Valle de Aburra ")

remove(Hurtos_ANT, df_mensual_Rio, df_mensual_med, df_mensual_Envi, df_mensual_antioqua,
       df_mensual_Bello, df_mensual_Valle, anio_inicio, mes_inicio, serie_ts_ant, serie_ts_Rio,
       serie_ts_med, serie_ts_Envi, serie_ts_Bello, serie_ts_Valle)
