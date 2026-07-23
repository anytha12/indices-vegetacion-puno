# ================================================================
# INTEGRACIÓN DE ÍNDICES DE VEGETACIÓN Y ESTADÍSTICA ESPACIAL
# REGIÓN PUNO, 2020-2025
# Los gráficos se muestran en RStudio y no se guardan como imágenes.
# ================================================================

# 1. PAQUETES -----------------------------------------------------

paquetes <- c("tidyverse", "sf", "spdep", "gstat", "corrplot")

no_instalados <- paquetes[!paquetes %in% rownames(installed.packages())]

if (length(no_instalados) > 0) {
  install.packages(no_instalados, dependencies = TRUE)
}

invisible(lapply(paquetes, library, character.only = TRUE))

set.seed(123)
options(scipen = 999)

dir.create("resultados", showWarnings = FALSE)
dir.create("resultados/tablas", showWarnings = FALSE)
dir.create("resultados/mapas", showWarnings = FALSE)

# 2. IMPORTAR Y LIMPIAR LA BASE -----------------------------------

datos <- read_csv(
  "C:/Users/USER/Downloads/Espacial/base_puno_2020_2025.csv",
  show_col_types = FALSE
)

columnas_necesarias <- c(
  "departamento", "provincia", "anio",
  "longitud", "latitud",
  "NDVI", "SAVI", "EVI", "NDMI",
  "precipitacion_mm", "temperatura_c", "altitud_m"
)

faltantes <- setdiff(columnas_necesarias, names(datos))

if (length(faltantes) > 0) {
  stop(
    paste(
      "Faltan estas columnas en el CSV:",
      paste(faltantes, collapse = ", ")
    )
  )
}

datos_limpios <- datos %>%
  transmute(
    departamento = str_squish(as.character(departamento)),
    provincia = str_squish(as.character(provincia)),
    anio = as.integer(anio),
    longitud = as.numeric(longitud),
    latitud = as.numeric(latitud),
    NDVI = as.numeric(NDVI),
    SAVI = as.numeric(SAVI),
    EVI = as.numeric(EVI),
    NDMI = as.numeric(NDMI),
    precipitacion_mm = as.numeric(precipitacion_mm),
    temperatura_c = as.numeric(temperatura_c),
    altitud_m = as.numeric(altitud_m)
  ) %>%
  filter(
    complete.cases(.),
    between(anio, 2020, 2025),
    between(longitud, -72.5, -68.0),
    between(latitud, -17.5, -13.0),
    between(NDVI, -1, 1),
    between(SAVI, -1.5, 1.5),
    between(EVI, -1, 1),
    between(NDMI, -1, 1),
    precipitacion_mm >= 0,
    altitud_m >= 0
  ) %>%
  distinct()

cat("Registros originales:", nrow(datos), "\n")
cat("Registros utilizados:", nrow(datos_limpios), "\n")
cat("Provincias:", n_distinct(datos_limpios$provincia), "\n")
cat("Periodo:", min(datos_limpios$anio), "-", max(datos_limpios$anio), "\n")

write_csv(
  datos_limpios,
  "resultados/tablas/base_limpia.csv"
)

# 3. ESTADÍSTICA DESCRIPTIVA --------------------------------------

variables <- c(
  "NDVI", "SAVI", "EVI", "NDMI",
  "precipitacion_mm", "temperatura_c", "altitud_m"
)

descriptivos <- map_dfr(variables, function(variable) {
  x <- datos_limpios[[variable]]

  tibble(
    variable = variable,
    n = length(x),
    media = mean(x),
    mediana = median(x),
    desviacion = sd(x),
    minimo = min(x),
    maximo = max(x)
  )
})

print(descriptivos)

write_csv(
  descriptivos,
  "resultados/tablas/descriptivos_generales.csv"
)

resumen_provincia <- datos_limpios %>%
  group_by(provincia) %>%
  summarise(
    registros = n(),
    NDVI = mean(NDVI),
    SAVI = mean(SAVI),
    EVI = mean(EVI),
    NDMI = mean(NDMI),
    precipitacion_mm = mean(precipitacion_mm),
    temperatura_c = mean(temperatura_c),
    altitud_m = mean(altitud_m),
    .groups = "drop"
  ) %>%
  arrange(desc(NDVI))

print(resumen_provincia)

write_csv(
  resumen_provincia,
  "resultados/tablas/resumen_por_provincia.csv"
)

resumen_anual <- datos_limpios %>%
  group_by(anio) %>%
  summarise(
    NDVI = mean(NDVI),
    SAVI = mean(SAVI),
    EVI = mean(EVI),
    NDMI = mean(NDMI),
    precipitacion_mm = mean(precipitacion_mm),
    temperatura_c = mean(temperatura_c),
    .groups = "drop"
  )

print(resumen_anual)

write_csv(
  resumen_anual,
  "resultados/tablas/resumen_anual.csv"
)

# 4. COMPARACIÓN ENTRE PROVINCIAS Y AÑOS -------------------------

grafico_provincias <- resumen_provincia %>%
  select(provincia, NDVI, SAVI, EVI, NDMI) %>%
  pivot_longer(
    -provincia,
    names_to = "indice",
    values_to = "media"
  ) %>%
  ggplot(
    aes(
      x = reorder(provincia, media),
      y = media
    )
  ) +
  geom_col() +
  coord_flip() +
  facet_wrap(~indice, scales = "free_x") +
  labs(
    title = "Índices de vegetación por provincia",
    x = "Provincia",
    y = "Valor medio"
  ) +
  theme_minimal()

print(grafico_provincias)

grafico_anual <- resumen_anual %>%
  select(anio, NDVI, SAVI, EVI, NDMI) %>%
  pivot_longer(
    -anio,
    names_to = "indice",
    values_to = "media"
  ) %>%
  ggplot(
    aes(
      x = anio,
      y = media,
      group = indice,
      linetype = indice
    )
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = 2020:2025) +
  labs(
    title = "Evolución anual de los índices de vegetación",
    x = "Año",
    y = "Valor medio",
    linetype = "Índice"
  ) +
  theme_minimal()

print(grafico_anual)

# 5. RELACIÓN CON VARIABLES LOCALES -------------------------------

matriz_correlacion <- datos_limpios %>%
  select(all_of(variables)) %>%
  cor(use = "complete.obs", method = "pearson")

print(round(matriz_correlacion, 3))

write.csv(
  round(matriz_correlacion, 4),
  "resultados/tablas/matriz_correlacion.csv"
)

corrplot(
  matriz_correlacion,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  number.cex = 0.7
)

resultados_correlacion <- expand_grid(
  indice = c("NDVI", "SAVI", "EVI", "NDMI"),
  variable_local = c(
    "precipitacion_mm",
    "temperatura_c",
    "altitud_m"
  )
) %>%
  pmap_dfr(function(indice, variable_local) {
    prueba <- cor.test(
      datos_limpios[[indice]],
      datos_limpios[[variable_local]],
      method = "pearson"
    )

    tibble(
      indice = indice,
      variable_local = variable_local,
      correlacion = unname(prueba$estimate),
      p_valor = prueba$p.value
    )
  })

print(resultados_correlacion)

write_csv(
  resultados_correlacion,
  "resultados/tablas/correlaciones_indices_variables_locales.csv"
)

# 6. PREPARAR LOS DATOS ESPACIALES -------------------------------

datos_sf <- st_as_sf(
  datos_limpios,
  coords = c("longitud", "latitud"),
  crs = 4326,
  remove = FALSE
)

datos_utm <- st_transform(datos_sf, 32719)

# 7. MORAN GLOBAL -------------------------------------------------

calcular_moran <- function(datos_anio, variable, k = 8) {

  datos_validos <- datos_anio %>%
    filter(!is.na(.data[[variable]]))

  if (nrow(datos_validos) > 2500) {
    datos_validos <- slice_sample(datos_validos, n = 2500)
  }

  coordenadas <- st_coordinates(datos_validos)

  vecinos <- knearneigh(
    coordenadas,
    k = k
  )

  lista_vecinos <- knn2nb(vecinos)

  pesos <- nb2listw(
    lista_vecinos,
    style = "W",
    zero.policy = TRUE
  )

  prueba <- moran.test(
    datos_validos[[variable]],
    pesos,
    zero.policy = TRUE
  )

  tibble(
    anio = unique(datos_validos$anio),
    indice = variable,
    Moran_I = unname(
      prueba$estimate["Moran I statistic"]
    ),
    p_valor = prueba$p.value
  )
}

moran_resultados <- expand_grid(
  anio = 2020:2025,
  indice = c("NDVI", "SAVI", "EVI", "NDMI")
) %>%
  pmap_dfr(function(anio, indice) {
    datos_anio <- datos_utm %>%
      filter(.data$anio == anio)

    calcular_moran(
      datos_anio = datos_anio,
      variable = indice
    )
  })

print(moran_resultados)

write_csv(
  moran_resultados,
  "resultados/tablas/moran_global.csv"
)

# 8. KRIGING DE LOS CUATRO ÍNDICES -------------------------------

anio_kriging <- max(datos_utm$anio)

datos_kriging <- datos_utm %>%
  filter(anio == anio_kriging)

if (nrow(datos_kriging) > 1800) {
  datos_kriging <- datos_kriging %>%
    slice_sample(n = 1800)
}

limite <- datos_utm %>%
  filter(anio == anio_kriging) %>%
  summarise() %>%
  st_convex_hull()

malla <- st_make_grid(
  limite,
  cellsize = 5000,
  what = "centers"
)

malla_sf <- st_sf(geometry = malla) %>%
  st_filter(limite)

ejecutar_kriging <- function(variable) {

  formula_kriging <- as.formula(
    paste(variable, "~ 1")
  )

  datos_variable <- datos_kriging %>%
    filter(!is.na(.data[[variable]]))

  datos_sp <- as(datos_variable, "Spatial")

  variograma_experimental <- variogram(
    formula_kriging,
    datos_sp,
    cutoff = 300000,
    width = 15000
  )

  varianza <- var(
    datos_variable[[variable]]
  )

  candidatos <- list(
    Esferico = vgm(
      psill = varianza * 0.8,
      model = "Sph",
      range = 100000,
      nugget = varianza * 0.2
    ),
    Exponencial = vgm(
      psill = varianza * 0.8,
      model = "Exp",
      range = 100000,
      nugget = varianza * 0.2
    ),
    Gaussiano = vgm(
      psill = varianza * 0.8,
      model = "Gau",
      range = 100000,
      nugget = varianza * 0.2
    )
  )

  modelos_ajustados <- map(
    candidatos,
    ~fit.variogram(
      variograma_experimental,
      .x
    )
  )

  errores <- map_dbl(
    modelos_ajustados,
    ~attr(.x, "SSErr")
  )

  mejor_nombre <- names(
    which.min(errores)
  )

  mejor_modelo <- modelos_ajustados[
    [mejor_nombre]
  ]

  validacion <- krige.cv(
    formula_kriging,
    datos_sp,
    model = mejor_modelo,
    nfold = 5,
    nmax = 40
  )

  metricas <- tibble(
    indice = variable,
    anio = anio_kriging,
    modelo = mejor_nombre,
    ME = mean(
      validacion$residual,
      na.rm = TRUE
    ),
    MAE = mean(
      abs(validacion$residual),
      na.rm = TRUE
    ),
    RMSE = sqrt(
      mean(
        validacion$residual^2,
        na.rm = TRUE
      )
    ),
    correlacion = cor(
      validacion$observed,
      validacion$var1.pred,
      use = "complete.obs"
    )
  )

  prediccion <- krige(
    formula_kriging,
    locations = datos_sp,
    newdata = as(malla_sf, "Spatial"),
    model = mejor_modelo,
    nmax = 40
  ) %>%
    st_as_sf()

  write.csv(
    as.data.frame(mejor_modelo),
    paste0(
      "resultados/tablas/variograma_",
      tolower(variable),
      ".csv"
    ),
    row.names = FALSE
  )

  plot(
    variograma_experimental,
    mejor_modelo,
    main = paste(
      "Semivariograma de",
      variable,
      "-",
      anio_kriging
    )
  )

  mapa <- ggplot() +
    geom_sf(
      data = prediccion,
      aes(fill = var1.pred),
      shape = 21,
      size = 3,
      stroke = 0
    ) +
    geom_sf(
      data = limite,
      fill = NA,
      linewidth = 0.5
    ) +
    labs(
      title = paste(
        "Kriging ordinario de",
        variable,
        "en Puno,",
        anio_kriging
      ),
      fill = paste(variable, "estimado")
    ) +
    theme_minimal()

  print(mapa)

  st_write(
    prediccion,
    paste0(
      "resultados/mapas/kriging_",
      tolower(variable),
      ".gpkg"
    ),
    delete_dsn = TRUE,
    quiet = TRUE
  )

  metricas
}

metricas_kriging <- map_dfr(
  c("NDVI", "SAVI", "EVI", "NDMI"),
  ejecutar_kriging
)

print(metricas_kriging)

write_csv(
  metricas_kriging,
  "resultados/tablas/validacion_kriging.csv"
)

# 9. RESUMEN FINAL ------------------------------------------------

cat("\nANÁLISIS FINALIZADO\n")
cat(
  "Provincia con mayor NDVI:",
  resumen_provincia$provincia[1],
  "\n"
)
cat(
  "Los gráficos se muestran en la pestaña Plots de RStudio.\n"
)
