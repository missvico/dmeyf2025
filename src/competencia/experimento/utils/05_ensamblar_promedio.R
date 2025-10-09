library(lightgbm)
library(data.table)
source("/home/vickydiliscia/dmeyf2025/src/competencia/experimento/utils/06_realidad_inicializar.R")


ensamblar_promedio <- function(dataset,
                                         PARAM,
                                         campos_buenos,
                                         experimento,
                                         model_paths = NULL,
                                         models_dir = ".",
                                         pattern = "^modelo_.*\\.txt$",
                                         out_dir = ".",
                                         sep_out = "\t") {
  stopifnot(is.data.table(dataset))
  stopifnot(!is.null(PARAM$future_final))
  stopifnot(length(campos_buenos) > 0)

  # 1) Aplico el modelo (N modelos) a los datos del futuro
  dfuture <- dataset[foto_mes %in% PARAM$future_final]

  if (nrow(dfuture) == 0L)
    stop("dfuture quedó vacío: no hay filas con foto_mes en PARAM$future_final")

  # Selección de modelos
  if (is.null(model_paths)) {
    model_paths <- list.files(models_dir, pattern = pattern, full.names = TRUE)
  }
  if (length(model_paths) == 0L)
    stop("No se encontraron modelos .txt. Revisá 'model_paths' o 'models_dir' + 'pattern'.")

  # Matriz de features
  X <- data.matrix(dfuture[, ..campos_buenos])

  # Cargar modelos y predecir
  modelos <- lapply(model_paths, lgb.load)
  predicciones_mat <- sapply(modelos, function(m) predict(m, newdata = X))

  # Promedio fila a fila
  prediccion <- rowMeans(predicciones_mat)

  # 2) Inicializo el dataset drealidad
  drealidad <- realidad_inicializar(dfuture, PARAM)

  # 3) Tabla de predicción
  tb_prediccion <- dfuture[, .(numero_de_cliente, foto_mes)]
  tb_prediccion[, prob := prediccion]
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  # 4) Construyo nombre de archivo usando el experimento
  out_file <- file.path(out_dir, paste0("prediccion_", experimento, ".txt"))

  fwrite(tb_prediccion, file = out_file, sep = sep_out)

  message("✅ Archivo de predicciones guardado en: ", out_file)

  invisible(list(
    tb_prediccion = tb_prediccion,
    drealidad = drealidad,
    model_paths = model_paths,
    predicciones_mat = predicciones_mat,
    out_file = out_file
  ))
}