library(data.table)

crear_lags <- function(dataset,
                       id_col   = "numero_de_cliente",
                       time_col = "foto_mes",
                       exclude  = c("clase_ternaria"),
                       lags     = c(1, 2)) {
  stopifnot(is.data.table(dataset))

  cols_lagueables <- setdiff(names(dataset), c(id_col, time_col, exclude))
  if (!length(cols_lagueables)) {
    warning("No hay columnas lagueables.")
    return(dataset)
  }

  message(sprintf("Creando lags: %s", paste(lags, collapse = ", ")))

  creado <- 0L
  for (lag_n in lags) {
    new_names <- paste0(cols_lagueables, "_lag", lag_n)

    dataset[, (new_names) := shift(.SD, n = lag_n, type = "lag"),
            by = c(id_col),
            .SDcols = cols_lagueables]

    creado <- creado + sum(new_names %in% names(dataset))
  }

  message(sprintf("✅ Lags creados: %d", creado))
  return(dataset)
}

crear_delta_lags <- function(dataset,
                             id_col   = "numero_de_cliente",
                             time_col = "foto_mes",
                             exclude  = c("clase_ternaria"),
                             lags     = c(1, 2)) {
  stopifnot(is.data.table(dataset))

  cols_lagueables <- setdiff(names(dataset), c(id_col, time_col, exclude))
  if (!length(cols_lagueables)) {
    warning("No hay columnas lagueables para delta.")
    return(dataset)
  }

  message(sprintf("Creando delta-lags: %s", paste(lags, collapse = ", ")))

  creados <- 0L
  for (lag_n in lags) {
    for (vcol in cols_lagueables) {
      lag_col   <- paste0(vcol, "_lag", lag_n)
      delta_col <- paste0(vcol, "_delta", lag_n)
      if (lag_col %in% names(dataset)) {
        dataset[, (delta_col) := get(vcol) - get(lag_col)]
        creados <- creados + 1L
      }
    }
  }

  message(sprintf("✅ Delta-lags creados: %d", creados))
  return(dataset)
}

