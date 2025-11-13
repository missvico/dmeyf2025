library(data.table)

crear_lags <- function(dataset,
                       id_col = "numero_de_cliente",
                       time_col = "foto_mes",
                       exclude = c("clase_ternaria"),
                       lags = c(1, 2)) {
  stopifnot(is.data.table(dataset))

  setorder(dataset, get(id_col), get(time_col))

  cols_lagueables <- setdiff(names(dataset), c(id_col, time_col, exclude))
  message(sprintf("Creando lags: %s", paste(lags, collapse = ", ")))

  for (lag_n in lags) {
    dataset[, paste0(cols_lagueables, "_lag", lag_n) :=
              shift(.SD, n = lag_n, type = "lag"),
            by = id_col,
            .SDcols = cols_lagueables]
  }

  return(dataset)
}


crear_delta_lags <- function(dataset,
                             id_col = "numero_de_cliente",
                             time_col = "foto_mes",
                             exclude = c("clase_ternaria"),
                             lags = c(1, 2)) {
  stopifnot(is.data.table(dataset))

  cols_lagueables <- setdiff(names(dataset), c("numero_de_cliente", "foto_mes", exclude))
  message(sprintf("Creando delta-lags: %s", paste(lags, collapse = ", ")))

  for (lag_n in lags) {
    for (vcol in cols_lagueables) {
      lag_col <- paste0(vcol, "_lag", lag_n)
      if (lag_col %in% names(dataset)) {
        delta_col <- paste0(vcol, "_delta", lag_n)
        dataset[, (delta_col) := get(vcol) - get(lag_col)]
      }
    }
  }

  return(dataset)
}
