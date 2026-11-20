# Pronóstico del hurto mensual de automóviles en Medellín

Trabajo final de Series de tiempo univariadas (3009297), UNAL Medellín, 2026-II.
Equipo: Julián Esteban Ariza Caro, Andres Felipe Cadavid Sanguino y Sebastián Valencia Botero.

## Requisitos

- R 4.6 o superior.
- Quarto 1.5 o superior, con una distribución de LaTeX (TinyTeX o MiKTeX) para el PDF.

## Instalación

```r
# En la raíz del repositorio (abrir st-2026-final-valencia-ariza.Rproj)
install.packages("renv")   # si no está instalado
renv::restore()            # instala las versiones exactas de renv.lock
```

## Ejecución

```sh
Rscript ejecutar-todo.R              # reconstruye datos/procesados y corre las pruebas
Rscript ejecutar-todo.R --descargar  # además baja una copia nueva y la compara
```

## Estructura

```
├── README.md
├── renv.lock               entorno declarado
├── ejecutar-todo.R         script maestro
├── datos/
│   ├── crudos/             registros descargados de datos.gov.co, sin tocar
│   └── procesados/         series que produce el código, nunca editadas a mano
├── R/
│   ├── 01-descarga.R       descarga desde la API de datos.gov.co
│   ├── 02-limpieza.R       registros → serie mensual
│   └── util-*.R            funciones de los talleres del curso
├── tests/testthat/         pruebas de las funciones propias
├── exploratorio/           análisis exploratorio por municipio
└── informe/
    ├── trabajo-final.qmd   informe en Quarto
    └── referencias.bib
```

## Datos

Policía Nacional de Colombia, *Reporte hurto de motocicletas y automotores en el
departamento de Antioquia*, datos abiertos, conjunto
[`96zn-kpak`](https://www.datos.gov.co/Seguridad-y-Defensa/Reporte-Hurto-de-motocicletas-y-automotores-en-el-/96zn-kpak).
La copia versionada es del 30 de septiembre de 2026 y tiene 144 040 registros.
La serie del trabajo son los hurtos de automóviles en Medellín de enero de 2011 a
diciembre de 2025: 180 meses y 17 466 hurtos.
