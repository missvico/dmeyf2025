ratios <- list(
  rfinanciacion_master = c("Master_msaldototal","Master_mfinanciacion_limite"),
  rlimite_master       = c("Master_msaldototal","Master_mlimitecompra"),
  rfinanciacion_visa   = c("Visa_msaldototal","Visa_mfinanciacion_limite"),
  rlimite_visa         = c("Visa_msaldototal","Visa_mlimitecompra"),
  rfinanciacion_total = c("msaldototal_total","mfinanciacionlimite_total"),
  rlimite_total_usado        = c("msaldototal_total","mlimitecompra_total"),
  rsaldo_pesos               = c("msaldopesos_total","msaldototal_total"),
  rsaldo_dolares             = c("msaldodolares_total","msaldototal_total"),
  rconsumo_pesos             = c("mconsumopesos_total","mconsumototal_total"),
  rconsumo_dolares           = c("msaldodolares_total","mconsumototal_total"),
  rconsumo_total_limite= c("mconsumototal_total","mlimitecompra_total"),
  rpagado_pesos                = c("mpagopesos_total","mpagado_total"),
  rpagado_dolar              = c("mpagosdolares_total","mpagado_total"),
  radelanto_pesos            = c("madelantopesos_total","madelanto_total"),
  radelanto_dolares          = c("madelantodolares_total","madelanto_total")
)