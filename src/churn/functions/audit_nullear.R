library(data.table)

# Auditor para verificar "nullear" aplicado según variables_rotas
# - Chequea que NO queden ceros en los meses marcados por variable
# - Resume NAs y ceros dentro/afuera
# - Mantiene el tipo de la columna (info)
audit_nullear <- function(dt, variables_rotas, month_col = "foto_mes",
                          replace_all_in_month = FALSE) {
  stopifnot(is.data.table(dt), month_col %in% names(dt))

  # helpers seguros para conteos
  count_zeros <- function(x) {
    if (is.numeric(x)) sum(x == 0, na.rm = TRUE) else NA_integer_
  }
  count_nas <- function(x) sum(is.na(x))

  res_list <- lapply(names(variables_rotas), function(v){
    if (!v %in% names(dt)) {
      return(data.table(
        variable = v, tipo = NA_character_,
        meses_marcados = NA_character_,
        filas_marcadas = NA_integer_,
        ceros_en_marcados = NA_integer_,
        NA_en_marcados = NA_integer_,
        ceros_fuera = NA_integer_,
        NA_fuera = NA_integer_,
        ok_sin_ceros_marc = NA, 
        modo = if (replace_all_in_month) "nullear_todo_mes" else "nullear_solo_ceros",
        status = "missing_column"
      ))
    }

    meses <- variables_rotas[[v]]
    if (!length(meses)) {
      return(data.table(
        variable = v, tipo = class(dt[[v]])[1],
        meses_marcados = "", filas_marcadas = 0L,
        ceros_en_marcados = 0L, NA_en_marcados = 0L,
        ceros_fuera = count_zeros(dt[[v]]),
        NA_fuera = count_nas(dt[[v]]),
        ok_sin_ceros_marc = TRUE,
        modo = if (replace_all_in_month) "nullear_todo_mes" else "nullear_solo_ceros",
        status = "no_months"
      ))
    }

    dentro <- dt[get(month_col) %in% meses, ..v][[1]]
    fuera  <- dt[!get(month_col) %in% meses, ..v][[1]]
    cls    <- class(dt[[v]])[1]

    zeros_in  <- count_zeros(dentro)
    nas_in    <- count_nas(dentro)
    zeros_out <- count_zeros(fuera)
    nas_out   <- count_nas(fuera)

    data.table(
      variable = as.character(v),
      tipo = cls,
      meses_marcados = paste(sort(unique(meses)), collapse = ","),
      filas_marcadas = length(dentro),
      ceros_en_marcados = zeros_in,
      NA_en_marcados = nas_in,
      ceros_fuera = zeros_out,
      NA_fuera = nas_out,
      ok_sin_ceros_marc = is.na(zeros_in) || zeros_in == 0L,
      modo = if (replace_all_in_month) "nullear_todo_mes" else "nullear_solo_ceros",
      status = "ok"
    )
  })

  out <- rbindlist(res_list, fill = TRUE)

  # Orden: primero las problemáticas (ok_sin_ceros_marc == FALSE), luego alfabético
  out[, bad := fifelse(is.na(ok_sin_ceros_marc), TRUE, !ok_sin_ceros_marc)]
  setorder(out, -bad, variable)
  out[, bad := NULL][]
}

# Ejemplo de uso:
# res <- audit_nullear(dataset, variables_rotas, month_col = "foto_mes", replace_all_in_month = FALSE)
# res[!ok_sin_ceros_marc | status != "ok"]
# stopifnot(all(res$ok_sin_ceros_marc[res$status == "ok"]))