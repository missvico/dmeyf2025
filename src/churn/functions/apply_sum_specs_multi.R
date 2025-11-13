suma_segura <- function(dt, new_var, inputs,
                                    weights = NULL,
                                    na_as_zero = TRUE,
                                    all_na_to_na = TRUE) {
  stopifnot(is.data.table(dt))
  faltan <- setdiff(inputs, names(dt))
  if (length(faltan)) stop("Faltan columnas: ", paste(faltan, collapse = ", "))

  M <- as.matrix(dt[, ..inputs])
  storage.mode(M) <- "double"

  # aplicar pesos opcionales
  if (!is.null(weights)) {
    if (length(weights) != ncol(M)) stop("`weights` debe tener mismo largo que `inputs`")
    M <- sweep(M, 2L, as.numeric(weights), `*`)
  }

  rs <- suma_segura_n(M, na_as_zero = na_as_zero, all_na_to_na = all_na_to_na)
  dt[, (new_var) := rs]
  invisible(dt)
}


apply_sum_specs_multi <- function(dt,
                                  spec,
                                  from_file    = FALSE,
                                  sep_inputs   = ",",
                                  na_as_zero   = TRUE,
                                  all_na_to_na = TRUE,
                                  verbose      = TRUE) {
  stopifnot(is.data.table(dt))

  # Leer spec desde archivo si corresponde
  if (isTRUE(from_file)) {
    spec <- data.table::fread(spec)
  }
  stopifnot(is.data.table(spec))

  # Validaciones mínimas
  req <- c("new_var", "inputs", "active")
  miss <- setdiff(req, names(spec))
  if (length(miss)) stop("Faltan columnas en spec: ", paste(miss, collapse = ", "))

  S <- copy(spec)[active == TRUE]
  if (nrow(S) == 0L) {
    if (verbose) message("ℹ️ No hay filas activas en spec; nada que aplicar.")
    return(dt)
  }

  .split <- function(x, sep) trimws(unlist(strsplit(x, sep, fixed = TRUE)))

  for (i in seq_len(nrow(S))) {
    new_var <- S[i, new_var]
    inputs  <- .split(S[i, inputs], sep_inputs)
    inputs  <- inputs[nzchar(inputs)]

    faltan <- setdiff(inputs, names(dt))
    if (length(faltan)) {
      warning(sprintf("Para '%s' faltan columnas: %s. Salteo.",
                      new_var, paste(faltan, collapse = ", ")))
      next
    }

    # Matriz base
    M <- as.matrix(dt[, ..inputs])
    storage.mode(M) <- "double"

    # Suma segura multi-columna
    rs <- {
      rs0 <- if (na_as_zero) rowSums(M, na.rm = TRUE) else rowSums(M)
      if (all_na_to_na) {
        all_na <- apply(M, 1L, function(r) all(is.na(r)))
        rs0[all_na] <- NA_real_
      }
      rs0
    }

    dt[, (new_var) := rs]
    if (verbose) message(sprintf("✅ '%s' = suma_segura(%s)",
                                 new_var, paste(inputs, collapse = "+")))
  }

  return(dt)
}
