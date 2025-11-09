library(data.table)

# Convierte YYYYMM -> año*12 + mes (1..12)
.to_periodo <- function(foto_mes) {
  as.integer(floor(foto_mes / 100)) * 12L + (foto_mes %% 100L)
}

interpolar_faltantes <- function(
  DT,
  variables,            # vector de columnas numéricas O named list: list(var1 = c(meses...), var2 = c(...))
  meses = NULL,         # vector YYYYMM si 'variables' es vector
  id_col   = "numero_de_cliente",
  time_col = "foto_mes",
  require_both   = TRUE,   # exige vecino anterior y posterior
  only_if_na     = TRUE,   # solo rellena si el valor es NA (o 0 si treat_zero_as_na=TRUE)
  treat_zero_as_na = FALSE, # considerar 0 como NA para interpolar
  copy = TRUE
) {
  stopifnot(is.data.table(DT) || is.data.frame(DT))
  if (copy) DT <- data.table::copy(as.data.table(DT)) else setDT(DT)

  # Normalizar mapping variable -> meses
  if (is.list(variables) && !is.null(names(variables))) {
    var_to_months <- variables
  } else {
    if (is.null(meses)) stop("Si 'variables' es vector, debés pasar 'meses'.")
    var_to_months <- setNames(rep(list(meses), length(variables)), variables)
  }

  # Índice mensual consecutivo
  if (!is.integer(DT[[time_col]])) DT[, (time_col) := as.integer(get(time_col))]
  DT[, `_periodo` := .to_periodo(get(time_col))]

  # Orden estable
  setkeyv(DT, c(id_col, "_periodo"))

  # Helper: ¿es NA lógico considerando ceros?
  is_missing <- function(x) {
    if (treat_zero_as_na) is.na(x) | x == 0 else is.na(x)
  }

  # Procesar variable por variable
  for (var in names(var_to_months)) {
    meses_obj <- as.integer(var_to_months[[var]])

    # Trabajar por grupo de cliente
    DT[, {
      vals <- get(var)
      per  <- `_periodo`
      fm   <- get(time_col)

      # Posiciones objetivo (meses a corregir)
      mask_obj <- fm %in% meses_obj
      if (!any(mask_obj)) {
        NULL
      } else {
        idx <- which(mask_obj)
        # Cortes de runs consecutivos por periodo
        if (length(idx) == 0L) {
          NULL
        } else {
          diffs <- diff(per[idx])
          cortes <- which(diffs != 1L) + 1L
          runs <- split(idx, cumsum(c(1L, diffs != 1L)))

          for (run_idx in runs) {
            if (length(run_idx) == 0L) next

            left_pos  <- min(run_idx) - 1L
            right_pos <- max(run_idx) + 1L

            have_left  <- left_pos >= 1L
            have_right <- right_pos <= length(vals)

            if (require_both && !(have_left && have_right)) next

            v_left  <- if (have_left)  vals[left_pos]  else NA_real_
            v_right <- if (have_right) vals[right_pos] else NA_real_

            # Si requerimos ambos, deben ser válidos (no NA ni cero si treat_zero_as_na=TRUE)
            if (require_both && (is_missing(v_left) || is_missing(v_right))) next

            # Target positions (dentro del run) según only_if_na
            if (only_if_na) {
              target_inside <- which(is_missing(vals[run_idx]))
            } else {
              target_inside <- seq_along(run_idx)
            }
            if (length(target_inside) == 0L) next

            k <- length(run_idx)

            # Interpolación
            if (have_left && have_right && !(is_missing(v_left) || is_missing(v_right))) {
              # v(i) = v_left + (i/(k+1))*(v_right - v_left), i=1..k
              delta <- (v_right - v_left) / (k + 1)
              interp_all <- v_left + delta * seq_len(k)
            } else if (have_left && !is_missing(v_left)) {
              interp_all <- rep(v_left, k)
            } else if (have_right && !is_missing(v_right)) {
              interp_all <- rep(v_right, k)
            } else {
              next
            }

            # Escribir solo en posiciones objetivo del run
            write_pos <- run_idx[target_inside]
            vals[write_pos] <- interp_all[target_inside]
          }

          # Devolver columna modificada al grupo
          .(tmp_vals = vals)
        }
      }
    }, by = id_col][, (var) := tmp_vals][, tmp_vals := NULL]

    # El join by group anterior deja una columna extra ordenada; re-keyear
    setkeyv(DT, c(id_col, "_periodo"))
  }

  # Limpieza
  DT[, `_periodo` := NULL]
  DT[]
}

