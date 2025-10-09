source("/home/vickydiliscia/dmeyf2025/src/competencia/experimento/utils/07_realidad_evaluar.R")
source("/home/vickydiliscia/dmeyf2025/src/competencia/experimento/utils/06_realidad_inicializar.R")

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

comparar_semillas <- function(pfuture, pparam, tb_prediccion, cortes, semillas) {
  stopifnot(is.data.table(tb_prediccion))
  res_list <- vector("list", length(semillas))

  for (i in seq_along(semillas)) {
    sem <- semillas[i]
    # inyectar semilla para el split
    pparam$semilla_kaggle <- sem
    PARAM$semilla_kaggle  <- sem  # si tu función usa PARAM global, mantener esto

    # reconstruir drealidad con el split de ESTA semilla
    drealidad_i <- realidad_inicializar(pfuture, pparam)

    # comparar cortes
    res_i <- comparar_cortes(tb_prediccion, cortes, drealidad_i)
    res_i[, semilla := sem]
    res_list[[i]] <- res_i
  }

  rbindlist(res_list, use.names = TRUE, fill = TRUE)
}
library(ggplot2)

plot_semillas <- function(res) {
  dt <- melt(res, id.vars = c("semilla","corte"),
             measure.vars = c("public","private"),
             variable.name = "leaderboard", value.name = "ganancia")
  ggplot(dt, aes(x = corte, y = ganancia, color = factor(semilla), group = semilla)) +
    geom_line() + geom_point(size = 1.8) +
    facet_wrap(~leaderboard, scales = "free_y") +
    labs(title = "Ganancia por corte y semilla",
         x = "Corte (top-N)", y = "Ganancia", color = "Semilla") +
    theme_minimal(base_size = 12)
}

