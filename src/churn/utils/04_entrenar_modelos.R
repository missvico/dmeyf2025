entrenar_modelos <- function(dtrain, params, nrounds, seeds, tag = "exp") {
    
  stopifnot(is.list(params), length(seeds) > 0)
  
  out_dir <- getwd()
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  modelos <- list()
  
  for (s in seeds) {
    message(sprintf("Entrenando modelo con semilla %d ...", s))
    param_i <- modifyList(params, list(seed = s))
    
    modelo_i <- lgb.train(
      data    = dtrain,
      params  = param_i,
      nrounds = nrounds,
      verbose = -1
    )
    
    model_name <- sprintf("modelo_%s_seed%d.txt", tag, s)
    model_path <- file.path(out_dir, model_name)
    lgb.save(modelo_i, model_path)
    
    modelos[[as.character(s)]] <- list(
      seed  = s,
      path  = model_path,
      model = modelo_i
    )
    
    message(sprintf("Modelo guardado: %s", model_path))
  }
  
  invisible(modelos)
}

library(lightgbm)
library(data.table)

entrenar_modelos_paralelos <- function(dtrain, params, nrounds, seeds, tag="exp") {
  resultados <- vector("list", length(seeds))
  names(resultados) <- as.character(seeds)
  t_all0 <- Sys.time()

  # asegurar paralelismo interno máximo
  params$num_threads <- 8  

  # construir dataset solo una vez para eficiencia
  lgb.Dataset.construct(dtrain)

  for (i in seq_along(seeds)) {
    s <- seeds[i]
    cat(sprintf("[%s] seed %d → inicio\n", format(Sys.time(), "%H:%M:%S"), s))
    t0 <- Sys.time()

    # insertar semilla específica
    p_i <- modifyList(params, list(seed = s))

    # entrenar modelo
    m <- lgb.train(
      data    = dtrain,
      params  = p_i,
      nrounds = nrounds,
      verbose = -1
    )

    # guardar modelo
    fn <- sprintf("modelo_%s_seed%d.txt", tag, s)
    lgb.save(m, fn)

    # logging de tiempos
    secs <- as.numeric(difftime(Sys.time(), t0, units="secs"))
    cat(sprintf("[%s] seed %d → fin (%.1f s) | guardado en %s\n",
                format(Sys.time(), "%H:%M:%S"), s, secs, fn))

    resultados[[i]] <- list(seed = s, path = file.path(getwd(), fn), secs = secs)
  }

  cat(sprintf("[TOTAL] %.1f s\n", as.numeric(difftime(Sys.time(), t_all0, units="secs"))))
  invisible(resultados)
}

