library(data.table)

percentilizar_variables <- function(df,
                                             digits = 3,
                                             scale01 = TRUE,
                                             # Si en algún momento tenés lags, podés excluirlos también:
                                             exclude_lags_regex = NULL  # e.g. "(_lag\\d+|_deltalag\\d+)$"
) {
  dt <- as.data.table(copy(df))

  # 1) Exclusión por nombre exacto (tu lista)
  exclude_names <- c(
    "numero_de_cliente","foto_mes",
    "active_quarter","cliente_vip","internet",
    "cliente_edad","cliente_antiguedad",
    "tmobile_app","tcuentas",
    "Visa_delinquency","Visa_status",
    "Master_status","Master_deliquency"  # escrito tal cual lo pasaste
  )

  # 2) Exclusión por prefijos/patrones
  #    - ^c           → todas las que empiezan con c (cantidades)
  #    - ^Master_c    → cantidades Master_
  #    - ^Visa_c      → cantidades Visa_
  #    - ^Master_[fF] → días Master_ f/F
  #    - ^Visa_[fF]   → días Visa_ f/F
  exclude_prefix_regex <- paste(
    c("^c", "^Master_c", "^Visa_c", "^Master_[fF]", "^Visa_[fF]"),
    collapse = "|"
  )

  # 3) Construir set final de exclusión
  all_names  <- names(dt)
  excl_regex_hits <- all_names[ grepl(exclude_prefix_regex, all_names) ]
  if (!is.null(exclude_lags_regex)) {
    excl_regex_hits <- unique(c(excl_regex_hits, all_names[ grepl(exclude_lags_regex, all_names) ]))
  }
  exclude_all <- unique(c(exclude_names, excl_regex_hits))

  # 4) Columnas elegibles = numéricas no excluidas
  cols <- setdiff(all_names, exclude_all)
  num_cols <- cols[sapply(dt[, ..cols], is.numeric)]
  if (!length(num_cols)) {
    warning("No hay columnas numéricas elegibles para percentilizar después de aplicar exclusiones.")
    return(dt[])
  }

  # 5) Forzar numeric (double) para evitar truncados
  for (col in num_cols) set(dt, j = col, value = as.numeric(dt[[col]]))

  # 6) Percentilizar por foto_mes
  for (col in num_cols) {
    dt[, (col) := {
      x <- get(col)
      f <- ecdf(x[!is.na(x)])       # ECDF mensual
      p <- f(x)
      if (!scale01) p <- p * 100
      round(p, digits)
    }, by = foto_mes]
  }

  dt[]
}
