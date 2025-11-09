agregar_clase_ternaria <- function(dataset) {
  require(data.table)

  # Asegurar data.table sin copiar innecesariamente
  dataset <- as.data.table(dataset)

  # periodo0 consecutivo
  dsimple <- dataset[, .(
    pos = .I,
    numero_de_cliente,
    periodo0 = as.integer(foto_mes %/% 100) * 12 + (foto_mes %% 100)
  )]

  # Ordenar por cliente y periodo
  setorder(dsimple, numero_de_cliente, periodo0)

  # Topes
  periodo_ultimo     <- dsimple[, max(periodo0)]
  periodo_anteultimo <- periodo_ultimo - 1

  # Leads 1 y 2 por cliente (type="lead")
  dsimple[, c("periodo1","periodo2") :=
            shift(periodo0, n = 1:2, type = "lead"),
          by = numero_de_cliente]

  # Iniciar como NA (igual al script original)
  dsimple[, clase_ternaria := NA_character_]

  # CONTINUA (solo antes del anteúltimo)
  dsimple[ periodo0 < periodo_anteultimo, clase_ternaria := "CONTINUA" ]

  # BAJA+1
  dsimple[ periodo0 < periodo_ultimo &
             ( is.na(periodo1) | (periodo0 + 1 < periodo1) ),
           clase_ternaria := "BAJA+1" ]

  # BAJA+2
  dsimple[ periodo0 < periodo_anteultimo &
             (periodo0 + 1 == periodo1) &
             ( is.na(periodo2) | (periodo0 + 2 < periodo2) ),
           clase_ternaria := "BAJA+2" ]

  # Pegar resultado manteniendo orden original
  setorder(dsimple, pos)
  dataset[, clase_ternaria := dsimple$clase_ternaria ]

  dataset
}
