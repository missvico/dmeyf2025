library(data.table)

is_missing <- function(x) {
  if (is.character(x)) return(is.na(x) | trimws(x) == "")
  if (is.numeric(x))   return(is.na(x))
  if (is.logical(x))   return(is.na(x))
  is.na(x)
}

ver_meses_rotos <- function(dt, month_col = "foto_mes", exclude = NULL) {
  stopifnot(is.data.table(dt), month_col %in% names(dt))
  cols_eval <- setdiff(names(dt), c(month_col, exclude %||% character(0)))

  # Lógicos de NA y ceros
  m_na   <- dt[, lapply(.SD, is_missing), .SDcols = cols_eval]
  m_zero <- dt[, lapply(.SD, function(x) if (is.numeric(x)) x == 0 else FALSE), .SDcols = cols_eval]

  m_na[,   (month_col) := dt[[month_col]] ]
  m_zero[, (month_col) := dt[[month_col]] ]

  # % NA y % ceros por mes
  out_na   <- m_na[,   c(lapply(.SD, mean), list(n = .N)), by = month_col, .SDcols = cols_eval]
  out_zero <- m_zero[, c(lapply(.SD, mean), list(n = .N)), by = month_col, .SDcols = cols_eval]

  na_long   <- melt(out_na,   id.vars = c(month_col, "n"), variable.name = "variable", value.name = "pct_na")
  zero_long <- melt(out_zero, id.vars = c(month_col, "n"), variable.name = "variable", value.name = "pct_zero")

  out_long <- merge(na_long, zero_long, by = c(month_col, "variable", "n"))
  setcolorder(out_long, c(month_col, "variable", "pct_na", "pct_zero", "n"))

  # Agrego resumen general
  resumen <- out_long[, .(
    meses_total = .N,
    meses_100_na = sum(pct_na == 1),
    meses_100_cero = sum(pct_zero == 1),
    meses_50_na = sum(pct_na >= 0.5 & pct_na < 1),
    meses_50_cero = sum(pct_zero >= 0.5 & pct_zero < 1)
  ), by = variable][order(-meses_100_na, -meses_100_cero)]

  list(detalle = out_long[], resumen = resumen[])
}
