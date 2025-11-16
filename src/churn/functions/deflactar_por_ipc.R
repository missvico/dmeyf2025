deflactar_por_ipc <- function(dt,
                             ipc_dt,
                             time_col   = "foto_mes",
                             ipc_col    = "ipc",     # acá vienen las variaciones mensuales %
                             base_mes   = NULL,
                             money_vars = NULL,
                             suffix     = "_real",
                             replace    = FALSE) {

  stopifnot(is.data.table(dt), is.data.table(ipc_dt))

  # Ordenar ipc_dt por tiempo para acumular bien
  setorderv(ipc_dt, time_col)

  # 1) Construir índice acumulado a partir de variaciones mensuales (%)
  #    indice_t = 100 * Π (1 + ipc_t/100)
  ipc_dt[, indice := 100 * cumprod(1 + get(ipc_col) / 100)]

  # 2) Elegir base (último mes si no se pasa)
  if (is.null(base_mes)) {
    base_mes <- max(ipc_dt[[time_col]])
  }

  base_indice <- ipc_dt[get(time_col) == base_mes, indice]
  if (length(base_indice) != 1L) {
    stop("No se pudo determinar un único índice base para base_mes = ", base_mes)
  }

  # 3) Deflactor clásico: indice_base / indice_t
  ipc_dt[, defl_factor := base_indice / indice]

  # 4) Join por foto_mes
  setkeyv(ipc_dt, time_col)
  setkeyv(dt, time_col)
  dt <- ipc_dt[dt]

  # 5) Detectar money_vars si no se pasan
  if (is.null(money_vars)) {
    money_vars <- grep("^(m|Master_m|Visa_m)", names(dt), value = TRUE)
  }

  # 6) Aplicar deflactor
  for (v in money_vars) {
    if (!v %in% names(dt)) next

    new_col <- if (replace) v else paste0(v, suffix)

    dt[, (new_col) := get(v) * defl_factor]
  }

  # 7) Limpieza opcional
  if (replace) {
    dt[, c(ipc_col, "indice", "defl_factor") := NULL]
  }

  dt[]
}


