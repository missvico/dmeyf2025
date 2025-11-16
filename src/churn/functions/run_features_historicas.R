

#spec_vars <- data.table::fread(text = "", header = TRUE)

# Ordená tu dataset
#setorder(dt, numero_de_cliente, foto_mes)

# Corre las features “por variable”
#run_feature_plan_vars(
#  dt, spec_vars,
#  id_col = "numero_de_cliente",
#  time_col = "foto_mes",
#  suffix = "m",
#  min_obs = 1,                    # clave para < ventana
#  only_at = NULL                  # o p.ej. max(dt$foto_mes)
#)

# (Opcional) Corre los pares
#run_feature_plan_pairs(dt, spec_pairs)