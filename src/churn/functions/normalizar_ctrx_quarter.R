library(data.table)

normalizar_ctrx_quarter <- function(dataset,
                                    var_ctr = "ctrx_quarter",
                                    var_antig = "cliente_antiguedad",
                                    nueva_col = "ctrx_quarter_normalizado") {
  stopifnot(is.data.table(dataset))
  stopifnot(var_ctr %in% names(dataset), var_antig %in% names(dataset))

  dataset[, (nueva_col) := as.numeric(get(var_ctr))]

  dataset[get(var_antig) == 1, (nueva_col) := get(var_ctr) * 5.0]
  dataset[get(var_antig) == 2, (nueva_col) := get(var_ctr) * 2.0]
  dataset[get(var_antig) == 3, (nueva_col) := get(var_ctr) * 1.2]

  return(dataset)
}
