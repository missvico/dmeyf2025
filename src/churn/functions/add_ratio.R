# División segura para proporciones
# --------------------------------
# numer, denom: vectores numéricos (longitud reciclable como en R)
# on_zero: qué devolver si denom == 0      -> "NA", "0", "Inf", "Nan", "keep" (mantener Inf/-Inf)
# on_na:   qué hacer si numer o denom es NA -> "NA", "0"
# eps:     si >0, trata |denom| < eps como cero numérico (estabiliza)
# clip:    opcional, vector c(min, max) para recortar la razón (p.ej., c(0,1))
safe_div <- function(numer, denom,
                     on_zero = c("NA", "0", "Inf", "Nan", "keep"),
                     on_na   = c("NA", "0"),
                     eps = 0,
                     clip = NULL) {
  on_zero <- match.arg(on_zero)
  on_na   <- match.arg(on_na)

  # Vector resultado base: división cruda
  out <- numer / denom

  # Normalizar denominador "casi cero" si eps > 0
  if (!is.null(eps) && eps > 0) {
    denom_is_small <- is.finite(denom) & abs(denom) < eps
  } else {
    denom_is_small <- rep(FALSE, length.out = max(length(numer), length(denom)))
  }

  # Ceros "efectivos": cero exacto o casi cero por eps
  denom_is_zero <- (denom == 0) | denom_is_small

  # Casos NA en entradas
  na_inputs <- is.na(numer) | is.na(denom)

  # Resolver NA de entrada
  if (any(na_inputs)) {
    if (on_na == "NA") {
      out[na_inputs] <- NA_real_
    } else if (on_na == "0") {
      out[na_inputs] <- 0
    }
  }

  # Resolver denominador cero / casi cero
  if (any(denom_is_zero, na.rm = TRUE)) {
    idx <- denom_is_zero & !na_inputs
    if (any(idx)) {
      out[idx] <- switch(on_zero,
        "NA"  = NA_real_,
        "0"   = 0,
        "Inf" = ifelse(numer[idx] >= 0,  Inf, -Inf),
        "Nan" = NaN,
        "keep"= out[idx]  # deja lo que resulte (Inf/-Inf) de numer/0
      )
    }
  }

  # Recorte opcional
  if (!is.null(clip) && length(clip) == 2L) {
    out <- pmax(clip[1], pmin(clip[2], out))
  }

  out
}

library(data.table)

# Crea una columna ratio nueva en DT = numer/denom usando safe_div
add_ratio_dt <- function(DT, numer, denom, out_col = NULL, ...) {
  stopifnot(is.data.table(DT))
  if (is.null(out_col)) {
    out_col <- paste0(numer, "_per_", denom)
  }
  DT[, (out_col) := safe_div(get(numer), get(denom), ...)]
  invisible(DT)
}

