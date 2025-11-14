if (!require("Rcpp")) install.packages("Rcpp")
library(Rcpp)

Rcpp::sourceCpp("/home/vickydiliscia/dmeyf2025/src/churn/functions/fhist_v2.cpp")

apply_rolling_with_kernel <- function(dt,
                                      specs,
                                      id_col   = "numero_de_cliente",
                                      time_col = "foto_mes",
                                      suffix   = "m") {
  stopifnot(is.data.table(dt))

  # Aseguro specs como data.table
  if (!is.data.table(specs)) specs <- as.data.table(specs)

  # Normalización de active
  if (!"active" %in% names(specs)) {
    specs[, active := TRUE]
  } else {
    specs[, active := as.logical(active)]
  }

  if (!"ops" %in% names(specs)) {
    stop("El objeto specs debe tener una columna 'ops'")
  }
  if (!"var" %in% names(specs) || !"win" %in% names(specs)) {
    stop("El objeto specs debe tener columnas 'var' y 'win'")
  }

  # Ordenar UNA sola vez
  setorderv(dt, c(id_col, time_col))

  # Recorro cada fila de specs
  for (i in seq_len(nrow(specs))) {
    if (!isTRUE(specs$active[i])) next

    var     <- specs$var[i]
    win     <- as.integer(specs$win[i])
    ops_str <- specs$ops[i]

    if (is.na(ops_str) || ops_str == "") next
    if (!var %in% names(dt)) {
      warning(sprintf("Variable '%s' no está en dt; se omite fila %d de specs.", var, i))
      next
    }

    ops <- trimws(strsplit(ops_str, ",")[[1]])

    message(sprintf("Kernel Rcpp -> Var: %s | win: %d | ops: %s",
                    var, win, paste(ops, collapse = ", ")))

    # Extraigo vectores
    x  <- dt[[var]]
    id <- as.integer(dt[[id_col]])

    # Llamo al kernel UNA sola vez
    res <- roll_stats_kernel(x, id, win)

    # Nombres estándar
    mean_col   <- .make_name(var, "mean",           win, suffix)
    sd_col     <- .make_name(var, "sd",             win, suffix)
    cv_col     <- .make_name(var, "cv",             win, suffix)
    min_col    <- .make_name(var, "min",            win, suffix)
    max_col    <- .make_name(var, "max",            win, suffix)
    prop_col   <- .make_name(var, "prop_gt0",       win, suffix)
    streak_col <- .make_name(var, "max_streak_gt0", win, suffix)
    ratioavg_col <- .make_name(var, "ratioavg",     win, suffix)

    # Solo escribo las columnas que realmente pedís en ops

    if ("mean" %in% ops) {
      dt[, (mean_col) := res[, "mean"]]
    }

    if ("sd" %in% ops) {
      dt[, (sd_col) := res[, "sd"]]
    }

    if ("cv" %in% ops) {
      dt[, (cv_col) := res[, "cv"]]
    }

    # min / max no aparecen en tu CSV actual, pero los dejo por si después agregás "min" o "max"
    if ("min" %in% ops) {
      dt[, (min_col) := res[, "min"]]
    }

    if ("max" %in% ops) {
      dt[, (max_col) := res[, "max"]]
    }

    if ("prop_gt0" %in% ops) {
      dt[, (prop_col) := res[, "prop_gt0"]]
    }

    if ("max_streak_gt0" %in% ops) {
      dt[, (streak_col) := res[, "max_streak_gt0"]]
    }

    # 🔹 ratioavg = valor actual / mean rolling (aunque no hayas pedido "mean")
    if ("ratioavg" %in% ops) {
      # usamos el mean del kernel, no recalculamos nada
      mvec <- res[, "mean"]
      dt[, (ratioavg_col) :=
            fifelse(is.na(mvec) | mvec == 0, NA_real_, x / mvec)]
    }
  }

  invisible(dt)
}
