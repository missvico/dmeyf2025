library(data.table)

agregar_clase_ternaria <- function(dataset) {
  # Aseguramos que sea data.table
  dataset <- as.data.table(dataset)
  
  # Calculo el periodo0 consecutivo
  dsimple <- dataset[, .(
    pos = .I,
    numero_de_cliente,
    periodo0 = as.integer(foto_mes / 100) * 12 + foto_mes %% 100
  )]
  
  # Ordeno
  setorder(dsimple, numero_de_cliente, periodo0)
  
  # Calculo topes
  periodo_ultimo <- dsimple[, max(periodo0)]
  periodo_anteultimo <- periodo_ultimo - 1
  
  # Calculo los leads de orden 1 y 2
  dsimple[, c("periodo1", "periodo2") := shift(periodo0, n = 1:2, fill = NA, type = "lead"),
          by = numero_de_cliente]
  
  # Asigno clase_ternaria
  dsimple[, clase_ternaria := "CONTINUA"]  # valor por defecto
  
  # Calculo BAJA+1
  dsimple[periodo0 < periodo_ultimo &
            (is.na(periodo1) | periodo0 + 1 < periodo1),
          clase_ternaria := "BAJA+1"]
  
  # Calculo BAJA+2
  dsimple[periodo0 < periodo_anteultimo &
            (periodo0 + 1 == periodo1) &
            (is.na(periodo2) | periodo0 + 2 < periodo2),
          clase_ternaria := "BAJA+2"]
  
  # Pego el resultado en el dataset original
  setorder(dsimple, pos)
  dataset[, clase_ternaria := dsimple$clase_ternaria]
  
  # Devuelvo el dataset con la nueva columna
  return(dataset)
}
