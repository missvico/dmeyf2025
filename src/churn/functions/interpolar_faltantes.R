library(data.table)

.to_periodo <- function(foto_mes) {
  as.integer(floor(foto_mes / 100)) * 12L + (foto_mes %% 100L)
}

interpolar_faltantes <- function(
  DT,
  variables,                 # vector de columnas O named list: list(var1=c(YYYYMM,...), var2=c(...))
  meses = NULL,              # vector YYYYMM si 'variables' es vector
  id_col   = "numero_de_cliente",
  time_col = "foto_mes",
  require_both     = TRUE,   # exige vecino anterior y posterior
  only_if_na       = TRUE,   # solo pisa si es NA (o 0 si treat_zero_as_na=TRUE)
  treat_zero_as_na = FALSE,  # considerar 0 como NA para interpolar
  round_integers   = FALSE,  # si la col era integer, volver a integer con round()
  integer_prefix   = "^c",   # 👈 variables que deben quedar enteras (conteos)
  copy             = TRUE
) {
  stopifnot(is.data.table(DT) || is.data.frame(DT))
  if (copy) DT <- data.table::copy(as.data.table(DT)) else setDT(DT)

  # Normalizar mapping variable -> meses
  if (is.list(variables) && !is.null(names(variables))) {
    var_to_months <- variables
  } else {
    if (is.null(meses)) stop("Si 'variables' es vector, debés pasar 'meses'.")
    var_to_months <- setNames(rep(list(as.integer(meses)), length(variables)), variables)
  }

  # foto_mes entero y periodo consecutivo
  if (!is.integer(DT[[time_col]])) DT[, (time_col) := as.integer(get(time_col))]
  DT[, `_periodo` := .to_periodo(get(time_col))]
  setkeyv(DT, c(id_col, "_periodo"))

  is_missing <- function(x) if (treat_zero_as_na) is.na(x) | x == 0 else is.na(x)

  for (var in names(var_to_months)) {
    meses_obj <- as.integer(var_to_months[[var]])
    orig_is_integer <- is.integer(DT[[var]])

    # 👇 debe quedar entero si:
    #   - su nombre matchea el prefijo (p.ej. "^c"), o
    #   - pediste round_integers y originalmente era integer
    must_integer <- (!is.null(integer_prefix) && nzchar(integer_prefix) && grepl(integer_prefix, var)) ||
                    (round_integers && orig_is_integer)

    # Evitar warnings/truncamientos: si la col es integer y la vamos a tocar, volverla numeric TEMPORALMENTE
    if (must_integer && is.integer(DT[[var]])) {
      DT[, (var) := as.numeric(get(var))]
    }

    # Interpolación por grupo (siempre trabaja en double)
    DT[, (var) := {
      vals <- as.numeric(get(var))
      per  <- `_periodo`
      fm   <- get(time_col)

      mask_obj <- fm %in% meses_obj
      if (any(mask_obj)) {
        idx <- which(mask_obj)
        if (length(idx) > 0L) {
          diffs <- diff(per[idx])
          runs  <- split(idx, cumsum(c(1L, diffs != 1L)))

          for (run_idx in runs) {
            if (length(run_idx) == 0L) next
            left_pos  <- min(run_idx) - 1L
            right_pos <- max(run_idx) + 1L
            have_left  <- left_pos  >= 1L
            have_right <- right_pos <= length(vals)
            if (require_both && !(have_left && have_right)) next

            v_left  <- if (have_left)  vals[left_pos]  else NA_real_
            v_right <- if (have_right) vals[right_pos] else NA_real_
            if (require_both && (is_missing(v_left) || is_missing(v_right))) next

            target_inside <- if (only_if_na) which(is_missing(vals[run_idx])) else seq_along(run_idx)
            if (length(target_inside) == 0L) next

            k <- length(run_idx)
            if (have_left && have_right && !(is_missing(v_left) || is_missing(v_right))) {
              delta      <- (v_right - v_left) / (k + 1)
              interp_all <- v_left + delta * seq_len(k)
            } else if (have_left && !is_missing(v_left)) {
              interp_all <- rep(v_left, k)
            } else if (have_right && !is_missing(v_right)) {
              interp_all <- rep(v_right, k)
            } else next

            write_pos <- run_idx[target_inside]
            vals[write_pos] <- interp_all[target_inside]
          }
        }
      }
      vals
    }, by = id_col]

    # 👇 Volver a ENTERO (redondeado) si corresponde
    if (must_integer) {
      DT[, (var) := as.integer(round(get(var)))]
    } else if (round_integers && orig_is_integer) {
      # Caso: pediste preservar integer solo si originalmente lo era
      DT[, (var) := as.integer(round(get(var)))]
    }
  }

  DT[, `_periodo` := NULL]
  DT[]
}

