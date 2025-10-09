source("/home/vickydiliscia/dmeyf2025/src/competencia/experimento/utils/07_realidad_evaluar.R")
library(data.table)
library(ggplot2)

comparar_cortes <- function(tb_prediccion, cortes, drealidad) {
  stopifnot(is.data.table(tb_prediccion), "prob" %in% names(tb_prediccion))
  tb <- copy(tb_prediccion)

  # ordenar una sola vez por prob desc
  setorder(tb, -prob, na.last = TRUE)

  res_list <- lapply(cortes, function(n) {
    # marcar top-N
    tb[, Predicted := 0L]
    if (n > 0L) tb[seq_len(min(n, .N)), Predicted := 1L]

    # evaluar
    sc <- realidad_evaluar(drealidad, tb)

    data.table(
      corte   = n,
      public  = sc$public,
      private = sc$private,
      total   = sc$total
    )
  })

  rbindlist(res_list)[order(corte)]
}

plot_cortes <- function(res) {
  dt <- melt(res, id.vars = "corte", measure.vars = c("public", "private"))
  ggplot(dt, aes(x = corte, y = value, group = variable)) +
    geom_line() + geom_point() +
    facet_wrap(~variable, scales = "free_y") +
    labs(title = "Ganancia por corte", x = "Corte (top-N)", y = "Ganancia") +
    theme_minimal(base_size = 12)
}


comparar_semillas <- function(tb_prediccion, cortes, drealidad, semillas) {
  stopifnot(is.data.table(tb_prediccion))
  resultados <- list()

  for (sem in semillas) {
    set.seed(sem)
    tb <- copy(tb_prediccion)
    setorder(tb, -prob, na.last = TRUE)

    res_list <- lapply(cortes, function(n) {
      tb[, Predicted := 0L]
      if (n > 0L) tb[seq_len(min(n, .N)), Predicted := 1L]

      sc <- realidad_evaluar(drealidad, tb)

      data.table(
        semilla = sem,
        corte   = n,
        public  = sc$public,
        private = sc$private,
        total   = sc$total
      )
    })

    resultados[[as.character(sem)]] <- rbindlist(res_list)
  }

  rbindlist(resultados)
}

plot_semillas <- function(res) {
  dt <- melt(res, id.vars = c("semilla", "corte"),
             measure.vars = c("public", "private"))

  ggplot(dt, aes(x = corte, y = value, color = factor(semilla))) +
    geom_line() + geom_point() +
    facet_wrap(~variable, scales = "free_y") +
    labs(
      title = "Ganancia por corte y semilla",
      x = "Corte (top-N)",
      y = "Ganancia",
      color = "Semilla"
    ) +
    theme_minimal(base_size = 12)
}
