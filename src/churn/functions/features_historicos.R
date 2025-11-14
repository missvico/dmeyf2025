library(data.table)

# =========================
# Helpers
# =========================
.make_name <- function(var, op, win, suffix) sprintf("%s__%s_%d%s", var, op, win, suffix)

# =========================
# Nivel / variabilidad
# =========================
roll_mean_by <- function(dt, var, id_col, time_col, win, suffix = "m", min_obs = 1) {
  setorderv(dt, c(id_col, time_col))
  newn <- .make_name(var, "mean", win, suffix)
  dt[, (newn) := frollmean(get(var), n = win, align = "right", na.rm = TRUE, 
                           adaptive = FALSE), by = c(id_col)]
  invisible(dt)
}

roll_sum_by <- function(dt, var, id_col, time_col, win, suffix = "m", min_obs = 1) {
  setorderv(dt, c(id_col, time_col))
  newn <- .make_name(var, "sum", win, suffix)
  dt[, (newn) := frollsum(get(var), n = win, align = "right", na.rm = TRUE, 
                          adaptive = FALSE), by = c(id_col)]
  invisible(dt)
}

roll_sd_by <- function(dt, var, id_col, time_col, win, suffix = "m", min_obs = 1) {
  setorderv(dt, c(id_col, time_col))
  newn <- .make_name(var, "sd", win, suffix)
  dt[, (newn) := frollapply(get(var), n = win, align = "right",
                            FUN = function(z) sd(z, na.rm = TRUE)), by = c(id_col)]
  invisible(dt)
}

roll_cv_by <- function(dt, var, id_col, time_col, win, suffix = "m", min_obs = 1) {
  # cv = sd/mean (ambas rolling)
  roll_mean_by(dt, var, id_col, time_col, win, suffix, min_obs)
  roll_sd_by(dt, var, id_col, time_col, win, suffix, min_obs)
  mean_col <- .make_name(var, "mean", win, suffix)
  sd_col   <- .make_name(var, "sd",   win, suffix)
  newn     <- .make_name(var, "cv",   win, suffix)
  dt[, (newn) := fifelse(is.na(get(mean_col)) | get(mean_col) == 0, NA_real_, get(sd_col) / get(mean_col))]
  invisible(dt)
}

roll_max_by <- function(dt, var, id_col, time_col, win, suffix = "m", min_obs = 1) {
  setorderv(dt, c(id_col, time_col))
  newn <- .make_name(var, "max", win, suffix)
  dt[, (newn) := frollapply(get(var), n = win, align = "right",
                            FUN = function(z) suppressWarnings(max(z, na.rm = TRUE))), by = c(id_col)]
  invisible(dt)
}

roll_min_by <- function(dt, var, id_col, time_col, win, suffix = "m", min_obs = 1) {
  setorderv(dt, c(id_col, time_col))
  newn <- .make_name(var, "min", win, suffix)
  dt[, (newn) := frollapply(get(var), n = win, align = "right",
                            FUN = function(z) suppressWarnings(min(z, na.rm = TRUE))), by = c(id_col)]
  invisible(dt)
}

roll_range_by <- function(dt, var, id_col, time_col, win, suffix = "m", min_obs = 1) {
  roll_max_by(dt, var, id_col, time_col, win, suffix, min_obs)
  roll_min_by(dt, var, id_col, time_col, win, suffix, min_obs)
  max_col <- .make_name(var, "max", win, suffix)
  min_col <- .make_name(var, "min", win, suffix)
  newn    <- .make_name(var, "range", win, suffix)
  dt[, (newn) := get(max_col) - get(min_col)]
  invisible(dt)
}

roll_quantile_by <- function(dt, var, id_col, time_col, win, q = 0.9, suffix = "m", min_obs = 1) {
  stopifnot(q > 0 & q < 1)
  setorderv(dt, c(id_col, time_col))
  tag  <- paste0("p", round(q*100))
  newn <- .make_name(var, tag, win, suffix)
  dt[, (newn) := frollapply(get(var), n = win, align = "right",
                            FUN = function(z) as.numeric(quantile(z, probs = q, na.rm = TRUE, type = 7))), by = c(id_col)]
  invisible(dt)
}

roll_ratioavg_by <- function(dt, var, id_col, time_col, win,
                             suffix = "m", min_obs = 1) {
  setorderv(dt, c(id_col, time_col))
  
  # Nombre de la columna de promedio rolling
  mean_col <- .make_name(var, "mean", win, suffix)
  
  # Si el promedio todavía no existe, lo calculamos
  if (!mean_col %in% names(dt)) {
    roll_mean_by(dt, var, id_col, time_col, win, suffix, min_obs)
  }
  
  # Nombre de salida: var__ratioavg_6m
  newn <- .make_name(var, "ratioavg", win, suffix)
  
  dt[, (newn) :=
        fifelse(get(mean_col) %in% c(NA_real_, 0),
                NA_real_,
                get(var) / get(mean_col))]
  
  invisible(dt)
}


# =========================
# Proporciones / frecuencias (flags o umbrales)
# =========================
roll_prop_gt0_by <- function(dt, var, id_col, time_col, win, suffix = "m", min_obs = 1) {
  setorderv(dt, c(id_col, time_col))
  newn <- .make_name(var, "prop_gt0", win, suffix)
  dt[, (newn) := frollmean(as.integer(get(var) > 0), n = win, align = "right",
                           na.rm = TRUE), by = c(id_col)]
  invisible(dt)
}

roll_prop_eq0_by <- function(dt, var, id_col, time_col, win, suffix = "m", min_obs = 1) {
  setorderv(dt, c(id_col, time_col))
  newn <- .make_name(var, "prop_eq0", win, suffix)
  dt[, (newn) := frollmean(as.integer(get(var) == 0), n = win, align = "right",
                           na.rm = TRUE), by = c(id_col)]
  invisible(dt)
}

# opcional: proporción bajo percentil histórico (ej. consumo bajo)
roll_prop_below_quantile_by <- function(dt, var, id_col, time_col, win, q = 0.25, suffix = "m", min_obs = 1) {
  # Marca 1 si el valor del mes está < p(q) de la ventana (auto-cambia mes a mes)
  setorderv(dt, c(id_col, time_col))
  qcol <- .make_name(var, paste0("p", round(q*100)), win, suffix)
  if (!qcol %in% names(dt)) roll_quantile_by(dt, var, id_col, time_col, win, q, suffix, min_obs)
  newn <- .make_name(var, paste0("prop_below_p", round(q*100)), win, suffix)
  # Promedio de flags bajo p(q) en la ventana (rolling mean de 0/1)
  dt[, (newn) := frollmean(as.integer(get(var) < get(qcol)), n = win, align = "right",
                           na.rm = TRUE), by = c(id_col)]
  invisible(dt)
}

# =========================
# Ratios / shares (pares de variables)
# =========================
roll_ratio_by <- function(dt, numer, denom, id_col, time_col, win, name = NA_character_, suffix = "m", min_obs = 1) {
  setorderv(dt, c(id_col, time_col))
  num_sum <- paste0(numer, "__sum_", win, suffix)
  den_sum <- paste0(denom, "__sum_", win, suffix)
  if (!num_sum %in% names(dt)) roll_sum_by(dt, numer, id_col, time_col, win, suffix, min_obs)
  if (!den_sum %in% names(dt)) roll_sum_by(dt, denom, id_col, time_col, win, suffix, min_obs)
  outname <- ifelse(is.na(name) | name == "", 
                    sprintf("%s_over_%s__%d%s", numer, denom, win, suffix), name)
  dt[, (outname) := fifelse(get(den_sum) %in% c(NA_real_, 0), NA_real_, get(num_sum) / get(den_sum))]
  invisible(dt)
}

roll_share_total_by <- function(dt, a, b, id_col, time_col, win, name = NA_character_, suffix = "m", min_obs = 1) {
  # a_share = sum(a) / (sum(a)+sum(b))
  setorderv(dt, c(id_col, time_col))
  a_sum <- paste0(a, "__sum_", win, suffix)
  b_sum <- paste0(b, "__sum_", win, suffix)
  if (!a_sum %in% names(dt)) roll_sum_by(dt, a, id_col, time_col, win, suffix, min_obs)
  if (!b_sum %in% names(dt)) roll_sum_by(dt, b, id_col, time_col, win, suffix, min_obs)
  tot <- sprintf("%s_plus_%s__sum_%d%s", a, b, win, suffix)
  if (!tot %in% names(dt)) dt[, (tot) := get(a_sum) + get(b_sum)]
  outname <- ifelse(is.na(name) | name == "", 
                    sprintf("%s_share_of_%s_plus_%s__%d%s", a, a, b, win, suffix), name)
  dt[, (outname) := fifelse(get(tot) %in% c(NA_real_, 0), NA_real_, get(a_sum) / get(tot))]
  invisible(dt)
}

# =========================
# Streaks (consecutivos) — útil para flags de actividad
# =========================
roll_max_streak_gt0_by <- function(dt, var, id_col, time_col, win, suffix = "m", min_obs = 1) {
  # Longitud máxima de rachas consecutivas > 0 dentro de la ventana
  setorderv(dt, c(id_col, time_col))
  newn <- .make_name(var, "max_streak_gt0", win, suffix)
  dt[, (newn) := frollapply(as.integer(get(var) > 0), n = win, align = "right",
                            FUN = function(v){
                              cnt <- 0; best <- 0
                              for (x in v) {
                                if (is.na(x) || x == 0L) { cnt <- 0L } else { cnt <- cnt + 1L }
                                if (cnt > best) best <- cnt
                              }
                              best
                            }), by = c(id_col)]
  invisible(dt)
}


library(data.table)

# =========================
# Dispatcher principal
# =========================

apply_rolling_from_specs <- function(dt,
                                     specs,
                                     id_col   = "numero_de_cliente",
                                     time_col = "foto_mes",
                                     suffix   = "m",
                                     min_obs  = 1) {
  stopifnot(is.data.table(dt))

  # Aseguro que specs sea data.table
  if (!is.data.table(specs)) specs <- as.data.table(specs)

  # Normalizaciones básicas
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

  # Recorro cada fila de specs
  for (i in seq_len(nrow(specs))) {
    if (!isTRUE(specs$active[i])) next

    var <- specs$var[i]
    win <- as.integer(specs$win[i])
    ops_str <- specs$ops[i]

    if (is.na(ops_str) || ops_str == "") next

    ops <- strsplit(ops_str, ",")[[1]]
    ops <- trimws(ops)

    message(sprintf("Variable: %s | win: %d | ops: %s",
                    var, win, paste(ops, collapse = ", ")))

    for (op in ops) {
      # -----------------------------------------
      # Mapeo de ops -> funciones roll_*
      # -----------------------------------------
      if (op == "mean") {
        roll_mean_by(dt, var, id_col, time_col, win, suffix, min_obs)

      } else if (op == "sum") {
        roll_sum_by(dt, var, id_col, time_col, win, suffix, min_obs)

      } else if (op == "sd") {
        roll_sd_by(dt, var, id_col, time_col, win, suffix, min_obs)

      } else if (op == "cv") {
        roll_cv_by(dt, var, id_col, time_col, win, suffix, min_obs)

      } else if (op == "range") {
        roll_range_by(dt, var, id_col, time_col, win, suffix, min_obs)

      # Cuantiles tipo p90, p75, p10, etc.
      } else if (grepl("^p[0-9]+$", op)) {
        q_num <- as.numeric(sub("p", "", op))
        q <- q_num / 100
        roll_quantile_by(dt, var, id_col, time_col, win,
                         q = q, suffix = suffix, min_obs = min_obs)

      # Proporciones
      } else if (op == "prop_gt0") {
        roll_prop_gt0_by(dt, var, id_col, time_col, win, suffix, min_obs)

      } else if (op == "prop_eq0") {
        roll_prop_eq0_by(dt, var, id_col, time_col, win, suffix, min_obs)

      # Ejemplo: prop_below_p25, prop_below_p10, etc.
      } else if (grepl("^prop_below_p[0-9]+$", op)) {
        q_num <- as.numeric(sub("prop_below_p", "", op))
        q <- q_num / 100
        roll_prop_below_quantile_by(dt, var, id_col, time_col, win,
                                    q = q, suffix = suffix, min_obs = min_obs)

      # Streaks
      } else if (op == "max_streak_gt0") {
        roll_max_streak_gt0_by(dt, var, id_col, time_col, win, suffix, min_obs)

      # 🔹 Nuevo: ratio contra el promedio rolling
      } else if (op == "ratioavg") {
        roll_ratioavg_by(dt, var, id_col, time_col, win, suffix, min_obs)

      } else {
        warning(sprintf("Operación '%s' no reconocida para variable '%s'; se omite.",
                        op, var))
      }
    }
  }
  dt
}

# =========================
# Dispatcher eficiente
# =========================

apply_rolling_from_specs_eficiente <- function(dt,
                                     specs,
                                     id_col   = "numero_de_cliente",
                                     time_col = "foto_mes",
                                     suffix   = "m",
                                     min_obs  = 1) {
  stopifnot(is.data.table(dt))

  # Aseguro que specs sea data.table
  if (!is.data.table(specs)) specs <- as.data.table(specs)

  # Normalizaciones básicas
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

  # 🔹 Ordenar UNA sola vez por id + tiempo
  setorderv(dt, c(id_col, time_col))

  # Recorro cada fila de specs
  for (i in seq_len(nrow(specs))) {
    if (!isTRUE(specs$active[i])) next

    var <- specs$var[i]
    win <- as.integer(specs$win[i])
    ops_str <- specs$ops[i]

    if (is.na(ops_str) || ops_str == "") next

    ops <- strsplit(ops_str, ",")[[1]]
    ops <- trimws(ops)

    message(sprintf("Variable: %s | win: %d | ops: %s",
                    var, win, paste(ops, collapse = ", ")))

    for (op in ops) {

      # -----------------------------------------
      # Mapeo de ops -> funciones / cálculos
      # con REUSO de columnas ya creadas
      # -----------------------------------------

      if (op == "mean") {
        mean_col <- .make_name(var, "mean", win, suffix)
        if (!mean_col %in% names(dt)) {
          roll_mean_by(dt, var, id_col, time_col, win, suffix, min_obs)
        }

      } else if (op == "sum") {
        sum_col <- .make_name(var, "sum", win, suffix)
        if (!sum_col %in% names(dt)) {
          roll_sum_by(dt, var, id_col, time_col, win, suffix, min_obs)
        }

      } else if (op == "sd") {
        sd_col <- .make_name(var, "sd", win, suffix)
        if (!sd_col %in% names(dt)) {
          roll_sd_by(dt, var, id_col, time_col, win, suffix, min_obs)
        }

      } else if (op == "cv") {
        # cv = sd / mean, reusando columnas si existen
        mean_col <- .make_name(var, "mean", win, suffix)
        sd_col   <- .make_name(var, "sd",   win, suffix)
        cv_col   <- .make_name(var, "cv",   win, suffix)

        if (!mean_col %in% names(dt)) {
          roll_mean_by(dt, var, id_col, time_col, win, suffix, min_obs)
        }
        if (!sd_col %in% names(dt)) {
          roll_sd_by(dt, var, id_col, time_col, win, suffix, min_obs)
        }
        if (!cv_col %in% names(dt)) {
          dt[, (cv_col) :=
                fifelse(is.na(get(mean_col)) | get(mean_col) == 0,
                        NA_real_,
                        get(sd_col) / get(mean_col))]
        }

      } else if (op == "range") {
        # range = max - min, reusando max/min si existen
        max_col   <- .make_name(var, "max",   win, suffix)
        min_col   <- .make_name(var, "min",   win, suffix)
        range_col <- .make_name(var, "range", win, suffix)

        if (!max_col %in% names(dt)) {
          roll_max_by(dt, var, id_col, time_col, win, suffix, min_obs)
        }
        if (!min_col %in% names(dt)) {
          roll_min_by(dt, var, id_col, time_col, win, suffix, min_obs)
        }
        if (!range_col %in% names(dt)) {
          dt[, (range_col) := get(max_col) - get(min_col)]
        }

      # Cuantiles tipo p90, p75, p10, etc.
      } else if (grepl("^p[0-9]+$", op)) {
        q_num <- as.numeric(sub("p", "", op))
        q <- q_num / 100

        tag       <- paste0("p", q_num)
        quant_col <- .make_name(var, tag, win, suffix)

        if (!quant_col %in% names(dt)) {
          roll_quantile_by(dt, var, id_col, time_col, win,
                           q = q, suffix = suffix, min_obs = min_obs)
        }

      # Proporciones
      } else if (op == "prop_gt0") {
        prop_col <- .make_name(var, "prop_gt0", win, suffix)
        if (!prop_col %in% names(dt)) {
          roll_prop_gt0_by(dt, var, id_col, time_col, win, suffix, min_obs)
        }

      } else if (op == "prop_eq0") {
        prop_col <- .make_name(var, "prop_eq0", win, suffix)
        if (!prop_col %in% names(dt)) {
          roll_prop_eq0_by(dt, var, id_col, time_col, win, suffix, min_obs)
        }

      # Ejemplo: prop_below_p25, prop_below_p10, etc.
      } else if (grepl("^prop_below_p[0-9]+$", op)) {
        q_num <- as.numeric(sub("prop_below_p", "", op))
        q <- q_num / 100

        q_tag   <- paste0("p", q_num)
        q_col   <- .make_name(var, q_tag, win, suffix)
        propcol <- .make_name(var, paste0("prop_below_p", q_num), win, suffix)

        # Aseguro el cuantíl rolling previo
        if (!q_col %in% names(dt)) {
          roll_quantile_by(dt, var, id_col, time_col, win,
                           q = q, suffix = suffix, min_obs = min_obs)
        }

        if (!propcol %in% names(dt)) {
          dt[, (propcol) :=
                frollmean(as.integer(get(var) < get(q_col)),
                          n = win, align = "right", na.rm = TRUE),
             by = c(id_col)]
        }

      # Streaks
      } else if (op == "max_streak_gt0") {
        streak_col <- .make_name(var, "max_streak_gt0", win, suffix)
        if (!streak_col %in% names(dt)) {
          roll_max_streak_gt0_by(dt, var, id_col, time_col, win, suffix, min_obs)
        }

      # 🔹 Nuevo: ratio contra el promedio rolling
      } else if (op == "ratioavg") {
        mean_col      <- .make_name(var, "mean",     win, suffix)
        ratioavg_col  <- .make_name(var, "ratioavg", win, suffix)

        # Aseguro mean rolling
        if (!mean_col %in% names(dt)) {
          roll_mean_by(dt, var, id_col, time_col, win, suffix, min_obs)
        }

        if (!ratioavg_col %in% names(dt)) {
          dt[, (ratioavg_col) :=
                fifelse(get(mean_col) %in% c(NA_real_, 0),
                        NA_real_,
                        get(var) / get(mean_col))]
        }

      } else {
        warning(sprintf("Operación '%s' no reconocida para variable '%s'; se omite.",
                        op, var))
      }
    }
  }

  dt
}

