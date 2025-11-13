library(data.table)

# nullear respetando el tipo (integer/double/logical/character)
.na_like <- function(x) {
  if (is.integer(x))   return(NA_integer_)
  if (is.numeric(x))   return(NA_real_)
  if (is.logical(x))   return(NA)
  if (is.character(x)) return(NA_character_)
  NA  # fallback
}

aplicar_dataquality_zeros <- function(dataset,
                                      variables_rotas,        # named list: var -> c(meses)
                                      modo = c("interpolar", "nullear"),
                                      month_col = "foto_mes",
                                      replace_all_in_month = FALSE,  # solo para "nullear"
                                      # params que forwards a interpolar_faltantes (si corresponde)
                                      treat_zero_as_na = TRUE,
                                      require_both     = TRUE,
                                      only_if_na       = TRUE,
                                      integer_prefix   = "^c",
                                      round_integers   = TRUE) {
  stopifnot(is.data.table(dataset), month_col %in% names(dataset))
  modo <- match.arg(modo)

  if (modo == "interpolar") {

    dataset <- interpolar_faltantes(
      dataset,
      variables        = variables_rotas,
      treat_zero_as_na = treat_zero_as_na,
      require_both     = require_both,
      only_if_na       = only_if_na,
      integer_prefix   = integer_prefix,
      round_integers   = round_integers
    )
    return(dataset)
  }

  # ---- modo == "nullear": SOLO en los meses indicados por variable ----
  for (v in names(variables_rotas)) {
    if (!v %in% names(dataset)) {
      warning(sprintf("Variable '%s' no existe en dataset; se omite.", v))
      next
    }
    meses <- variables_rotas[[v]]
    if (!length(meses)) next

    # NA del tipo correcto para esa columna
    na_val <- .na_like(dataset[[v]])

    if (isTRUE(replace_all_in_month)) {
      # Nullear TODO el mes para esa variable (sin chequear si es 0)
      dataset[get(month_col) %in% meses, (v) := na_val]
    } else {
      # Nullear SOLO ceros en esos meses para esa variable
      # (no toca valores distintos de 0)
      dataset[get(month_col) %in% meses & get(v) == 0, (v) := na_val]
    }
  }
  dataset
}
