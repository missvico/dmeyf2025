flip <- function(dt, var, month_from = -Inf, month_to = Inf, time_col = "foto_mes") {
  stopifnot(data.table::is.data.table(dt))

  # Nombre de la columna a modificar
  v <- deparse(substitute(var))

  # 1) Limpieza: cualquier cosa que no sea 0/1 -> NA
  dt[, (v) := fifelse(get(v) %in% 0:1, as.integer(get(v)), NA_integer_)]

  # 2) Flip: sólo para el rango de meses indicado y con valor válido
  dt[
    get(time_col) >= month_from & get(time_col) <= month_to & !is.na(get(v)),
    (v) := 1L - get(v)
  ]

  invisible(dt)
}