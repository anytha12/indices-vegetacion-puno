Integración de índices de vegetación y estadística espacial en Puno, 2020-2025

Este repositorio contiene el código desarrollado en R para analizar la distribución espacial y temporal de los índices de vegetación NDVI, SAVI, EVI y NDMI en el departamento de Puno durante el periodo 2020-2025.


Objetivo del proyecto

Analizar la variación espacial y temporal de los índices de vegetación en las provincias del departamento de Puno y estudiar su relación con variables ambientales como la precipitación, la temperatura y la altitud.

Variables analizadas

NDVI

SAVI

EVI

NDMI

Precipitación

Temperatura

Altitud

Métodos utilizados

Limpieza y depuración de datos

Estadística descriptiva

Comparación por provincia y año

Correlación de Pearson

Índice de Moran global

Semivariogramas

Kriging ordinario

Validación cruzada

Fuentes de datos

Sentinel-2

CHIRPS

ERA5-Land

SRTM

Google Earth Engine

Archivos principales

analisis_puno.R: código principal del análisis

base_puno_2020_2025.csv: base de datos utilizada

README.md: descripción del proyecto

.gitignore: archivos que Git no debe subir

Paquetes de R utilizados

tidyverse
sf
spdep
gstat
corrplot

Cómo ejecutar el proyecto

Descargar o clonar este repositorio.

Abrir el archivo analisis_puno.R en RStudio.

Verificar la ruta del archivo CSV.

Ejecutar el código desde el inicio.

Revisar los gráficos en la pestaña Plots.

Revisar las tablas en la carpeta resultados.

Resultados

El código genera tablas descriptivas, resúmenes por provincia y año, matriz de correlación, resultados de Moran, semivariogramas, mapas de Kriging y métricas de validación cruzada.
