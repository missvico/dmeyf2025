library(data.table)

# Devuelve: named list  variable -> integer vector de meses (ej: 201910L, 202006L)
# Detecta "meses rotos" donde el % de ceros por variable y mes alcanza el umbral dado.
build_variables_rotas <- function(dataset,
                                  month_col = "foto_mes",
                                  exclude   = c("numero_de_cliente", "clase_ternaria"),
                                  zero_threshold = 1.0) {
  stopifnot(is.data.table(dataset), month_col %in% names(dataset))
  cols_eval <- setdiff(names(dataset), c(month_col, exclude))

  # Matriz lógica de ceros (solo numéricas); FALSE para no numéricas
  m_zero <- dataset[, lapply(.SD, function(x) if (is.numeric(x)) x == 0 else FALSE), .SDcols = cols_eval]
  m_zero[, (month_col) := dataset[[month_col]] ]

  # % de ceros por mes
  out_zero <- m_zero[, c(lapply(.SD, mean), list(n = .N)), by = month_col, .SDcols = cols_eval]

  # Long format
  zero_long <- melt(out_zero, id.vars = c(month_col, "n"),
                    variable.name = "variable", value.name = "pct_zero")

  # Filtrar meses "rotos" según umbral (por defecto, 100% ceros)
  roto <- zero_long[pct_zero >= zero_threshold]

  # Armar named list variable -> meses (ordenados, únicos, integer)
  if (nrow(roto) == 0L) return(setNames(vector("list", 0), character(0)))

  salida <- roto[, .(meses = as.integer(sort(unique(get(month_col))))), by = variable]
  # pasar variable a character por si llegó como factor
  salida[, variable := as.character(variable)]

  # split a named list
  vars <- split(salida$meses, salida$variable)
  # asegurar integer (L) en la salida
  vars <- lapply(vars, function(v) as.integer(v))
  vars
}
