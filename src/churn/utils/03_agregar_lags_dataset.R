library(data.table)

agregar_lags_dataset <- function(df,
                             orders = 1L,            # puede ser c(1,2,...)
                             include_deltas = FALSE, # TRUE para agregar x - lag_k(x)
                             only_numeric = TRUE     # laggea solo numéricas
){
  dt <- as.data.table(df)

  # Validaciones mínimas
  stopifnot(all(c("numero_de_cliente","foto_mes") %in% names(dt)))

  # Orden panel-temporal (asume foto_mes tipo entero YYYYMM o Date)
  setorderv(dt, c("numero_de_cliente","foto_mes"))

  # Columnas base a laguear: todo menos id/time/target y sin columnas ya lagueadas
  base_cols <- setdiff(names(dt), c("numero_de_cliente","foto_mes","clase_ternaria"))
  base_cols <- base_cols[!grepl("(_lag\\d+|_deltalag\\d+)$", base_cols)]

  if (only_numeric) {
    base_cols <- base_cols[sapply(dt[, ..base_cols], is.numeric)]
  }
  if (!length(base_cols)) stop("No hay columnas elegibles para lags.")

  # Generación de lags (y delta-lags opcional)
  for (k in orders) {
    lag_names   <- paste0(base_cols, "_lag", k)
    dt[, (lag_names) := lapply(.SD, shift, n = k, type = "lag"),
       by = "numero_de_cliente", .SDcols = base_cols]

    if (include_deltas) {
      delta_names <- paste0(base_cols, "_deltalag", k)
      dt[, (delta_names) := Map(function(x, y) x - y, mget(base_cols), mget(lag_names))]
    }
  }

  return(dt[])
}
