# ------------------------------------------------------------
# División segura para PROPORCIONES / PROMEDIOS / RATIOS
# ------------------------------------------------------------
# mode = "proporcion":   pensado para valores en [0,1]
#   - por defecto: denom==0 => 0 ;  clip a [0,1]
# mode = "promedio":     pensado para monto / conteo (promedio por evento)
#   - por defecto: denom==0 => 0 ;  sin clip
# mode = "ratio":        genérico; sin clip, denom==0 => NA
#
# Opciones avanzadas:
#  - on_zero: qué poner si denom es 0 (o casi 0 si eps>0): "0", "NA", "Inf", "Nan", "keep"
#  - on_na:   qué hacer si numer o denom es NA: "NA" o "0"
#  - eps:     umbral para tratar |denom|<eps como 0 (p.ej. 1e-9)
#  - clip:    recorta el resultado a [min,max] si se indica (p.ej., c(0,1))
library(data.table)

# --- Reutilizamos tu dividir (totalmente vectorizada) ---
dividir <- function(numer, denom,
                            mode = c("proporcion","promedio","ratio"),
                            on_zero = NULL,
                            on_na = c("NA","0"),
                            eps = 1e-9,
                            clip = NULL) {
  mode  <- match.arg(mode)
  on_na <- match.arg(on_na)
  if (is.null(on_zero)) {
    on_zero <- switch(mode,
      "proporcion" = "0",
      "promedio"   = "0",
      "ratio"      = "NA"
    )
  }
  if (is.null(clip) && mode == "proporcion") clip <- c(0,1)

  out <- numer / denom
  denom_is_small <- is.finite(denom) & abs(denom) < eps
  denom_is_zero  <- (denom == 0) | denom_is_small
  na_inputs      <- is.na(numer) | is.na(denom)

  if (any(na_inputs)) {
    out[na_inputs] <- if (on_na == "NA") NA_real_ else 0
  }
  if (any(denom_is_zero & !na_inputs, na.rm = TRUE)) {
    idx <- denom_is_zero & !na_inputs
    out[idx] <- switch(on_zero,
      "NA"  = NA_real_,
      "0"   = 0,
      "Inf" = ifelse(numer[idx] >= 0,  Inf, -Inf),
      "Nan" = NaN,
      "keep"= out[idx]
    )
  }
  if (!is.null(clip) && length(clip) == 2L) {
    out <- pmax(clip[1], pmin(clip[2], out))
  }
  out
}

# --- Helper para crear muchas columnas a la vez ---
# mapping: list(nueva_col = c("numer","denom"), ...)
# modes:   named character vector opcional con el modo por salida (proporcion/promedio/ratio)
agregar_proporciones <- function(DT, mapping,
                              default_mode = "promedio",
                              modes = NULL,
                              on_zero = NULL,
                              on_na = "NA",
                              eps = 1e-9,
                              clip = NULL,
                              overwrite = TRUE) {
  stopifnot(is.data.table(DT))
  for (out_col in names(mapping)) {
    pair <- mapping[[out_col]]
    if (length(pair) != 2L) stop(sprintf("'%s' no tiene c(numer, denom)", out_col))
    numer <- pair[1]; denom <- pair[2]

    mode_here <- if (!is.null(modes) && out_col %in% names(modes)) modes[[out_col]] else default_mode
    clip_here <- if (!is.null(clip)) clip else if (identical(mode_here, "proporcion")) c(0,1) else NULL
    onz_here  <- if (!is.null(on_zero)) on_zero else if (mode_here %in% c("proporcion","promedio")) "0" else "NA"

    if (!overwrite && out_col %in% names(DT)) next

    DT[, (out_col) := dividir(get(numer), get(denom),
                                      mode   = mode_here,
                                      on_zero= onz_here,
                                      on_na  = on_na,
                                      eps    = eps,
                                      clip   = clip_here)]
  }
  invisible(DT)
}
