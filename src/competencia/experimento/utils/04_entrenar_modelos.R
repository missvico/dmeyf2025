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


library(parallel)
library(lightgbm)

entrenar_modelos_paralelos <- function(
  dtrain,
  params,
  nrounds,
  seeds,
  tag = "exp",
  workers = 2,             # <-- 2 procesos en paralelo
  threads_per_worker = 4   # <-- 4 hilos LightGBM por proceso
) {
  stopifnot(is.list(params), length(seeds) > 0)
  params$num_threads <- threads_per_worker

  # función interna: entrena y guarda un modelo por semilla
  train_one <- function(s, dtrain, params, nrounds, tag) {
    t0 <- Sys.time()
    param_i <- modifyList(params, list(seed = s))
    booster <- lgb.train(
      data    = dtrain,
      params  = param_i,
      nrounds = nrounds,
      verbose = -1
    )
    fname <- sprintf("modelo_%s_seed%d.txt", tag, s)
    lgb.save(booster, fname)
    t1 <- Sys.time()
    list(
      seed  = s,
      path  = file.path(getwd(), fname),
      secs  = as.numeric(difftime(t1, t0, units = "secs"))
    )
  }

  seeds <- as.integer(seeds)
  message(sprintf(">> Lanzando %d workers; %d hilos LGBM por worker", workers, threads_per_worker))

  if (.Platform$OS.type == "unix") {
    res <- mclapply(
      X = seeds,
      FUN = function(s) train_one(s, dtrain, params, nrounds, tag),
      mc.cores = workers
    )
  } else {
    cl <- makeCluster(workers)
    on.exit(stopCluster(cl), add = TRUE)
    # exportar objetos y paquetes
    clusterExport(cl, varlist = c("dtrain","params","nrounds","tag","train_one"), envir = environment())
    clusterEvalQ(cl, { library(lightgbm) })
    res <- parLapply(cl, seeds, function(s) train_one(s, dtrain, params, nrounds, tag))
  }

  # logging final
  secs <- vapply(res, function(x) x$secs, numeric(1))
  message(sprintf(">> Tiempo por semilla (mediana): %.1f s | Total CPU ~ %.1f s",
                  median(secs), sum(secs)))
  invisible(res)
}
