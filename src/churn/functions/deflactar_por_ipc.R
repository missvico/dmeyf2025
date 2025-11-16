deflactar_por_ipc <- function(dt,
                             ipc_dt,
                             time_col   = "foto_mes",
                             ipc_col    = "ipc",
                             base_mes   = NULL,
                             money_vars = NULL,
                             suffix     = "_real") {
  stopifnot(is.data.table(dt), is.data.table(ipc_dt))

  # Elegir base (último mes si no se pasa)
  if (is.null(base_mes)) {
    base_mes <- max(ipc_dt[[time_col]])
  }

  base_ipc <- ipc_dt[get(time_col) == base_mes, get(ipc_col)]
  if (length(base_ipc) != 1L) {
    stop("No se pudo determinar un único IPC base para base_mes = ", base_mes)
  }

  ipc_dt[, defl_factor := base_ipc / get(ipc_col)]

  # Join por foto_mes
  setkeyv(ipc_dt, time_col)
  setkeyv(dt, time_col)
  dt <- ipc_dt[dt]

  # Detectar money_vars si no se pasan
  if (is.null(money_vars)) {
    money_vars <- grep("^(m|Master_m|Visa_m)", names(dt), value = TRUE)
  }

  # Aplicar deflactor
  for (v in money_vars) {
    if (!v %in% names(dt)) next
    newname <- paste0(v, suffix)
    dt[, (newname) := get(v) * defl_factor]
  }

  dt[]
}
