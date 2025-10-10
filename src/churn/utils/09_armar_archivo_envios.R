library(data.table)

library(data.table)

armar_archivo_envios <- function(pred,
                                 corte,
                                 experimento = "exp",
                                 out_dir = "kaggle",
                                 id_col = "numero_de_cliente",
                                 prob_col = "prob",
                                 filename = NULL,
                                 sep_in = "\t") {
  # pred puede ser data.table o path al archivo
  tb_pred <- if (is.character(pred)) {
    stopifnot(file.exists(pred))
    fread(pred, sep = sep_in)
  } else {
    stopifnot(data.table::is.data.table(pred))
    copy(pred)
  }

  # checks básicos
  if (!all(c(id_col, prob_col) %in% names(tb_pred))) {
    stop(
      sprintf("Faltan columnas. Esperaba: '%s' y '%s'. Nombres presentes: %s",
              id_col, prob_col, paste(names(tb_pred), collapse=", "))
    )
  }
  stopifnot(is.numeric(corte), corte >= 0)

  # ordenar por prob desc (forma programática)
  setorderv(tb_pred, cols = prob_col, order = -1L, na.last = TRUE)

  # vector Predicted 0/1
  tb_pred[, Predicted := 0L]
  if (corte > 0L) {
    n <- min(corte, nrow(tb_pred))
    tb_pred[seq_len(n), Predicted := 1L]
  }

  # armar salida
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  if (is.null(filename)) filename <- sprintf("KA%s_%s.csv", experimento, corte)
  out_path <- file.path(out_dir, filename)

  # grabar SOLO columnas requeridas por Kaggle (id + Predicted)
  cols_out <- c(id_col, "Predicted")
  fwrite(tb_pred[, ..cols_out], file = out_path, sep = ",")

  message("✅ Generado: ", out_path, " (corte=", corte, ")")
  invisible(list(path = out_path, tb = tb_pred[, ..cols_out]))
}

