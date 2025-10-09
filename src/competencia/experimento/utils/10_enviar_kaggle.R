library(data.table)

enviar_archivo <- function(pred,
                           corte,
                           experimento      = "exp",
                           competition, 
                           message_submit   = NULL,
                           id_col           = "numero_de_cliente",
                           prob_col         = "prob",
                           sep_in           = "\t",
                           keep_file        = TRUE,
                           out_dir          = "kaggle") {

  # 1) Leer predicción
  tb_pred <- if (is.character(pred)) {
    stopifnot(file.exists(pred))
    fread(pred, sep = sep_in)
  } else {
    stopifnot(data.table::is.data.table(pred))
    copy(pred)
  }

  # 2) Checks
  if (!all(c(id_col, prob_col) %in% names(tb_pred))) {
    stop(sprintf("Faltan columnas. Esperaba '%s' y '%s'. Presentes: %s",
                 id_col, prob_col, paste(names(tb_pred), collapse = ", ")))
  }
  stopifnot(is.numeric(corte), corte >= 0)

  # 3) Ordenar y marcar top-N
  setorderv(tb_pred, cols = prob_col, order = -1L, na.last = TRUE)
  tb_pred[, Predicted := 0L]
  if (corte > 0L) tb_pred[seq_len(min(corte, nrow(tb_pred))), Predicted := 1L]

  # 4) Escribir CSV
  filename <- sprintf("KA%s_%s.csv", experimento, corte)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  out_path <- file.path(out_dir, filename)

  # >>> FIX: conservar nombres exactos
  cols_out <- c(id_col, "Predicted")
  out_dt   <- tb_pred[, ..cols_out]
  stopifnot(identical(names(out_dt), cols_out))
  fwrite(out_dt, file = out_path, sep = ",")

  # sanity check (opcional)
  hdr <- names(fread(out_path, nrows = 0))
  stopifnot(identical(hdr, cols_out))

  # 5) Subir a Kaggle
  if (is.null(message_submit))
    message_submit <- sprintf("Exp=%s | Corte=%s | submit auto", experimento, corte)

  cmd <- sprintf('kaggle competitions submit -c %s -f "%s" -m "%s"',
                 competition, out_path, message_submit)

  cat("\n⬆️ Subiendo:", out_path, "\n", cmd, "\n")
  out_submit <- tryCatch(system(cmd, intern = TRUE), error = function(e) as.character(e))
  cat(paste(out_submit, collapse = "\n"), "\n")

  cat("\n📜 Últimas submissions:\n")
  subs <- tryCatch(system(sprintf('kaggle competitions submissions -c %s', competition),
                          intern = TRUE), error = function(e) character())
  cat(paste(utils::tail(subs, 20), collapse = "\n"), "\n")

  invisible(list(path_csv = out_path, cmd = cmd, submit_log = out_submit))
}

