lgbm <-  list(
  boosting= "gbdt",
  objective= "binary",
  metric= "custom",
  first_metric_only= FALSE,
  boost_from_average= TRUE,
  feature_pre_filter= FALSE,
  force_row_wise= TRUE,
  verbosity= -100,

  max_bin= 31L,

  num_iterations= 9999L, # dejo libre la cantidad de arboles, zLightGBM se detiene solo
  num_leaves= 999L, # dejo libre la cantidad de hojas, zLightGBM sabe cuando no hacer un split
  feature_fraction= 0.50 # un valor equilibrado, habra que probar alternativas ...
    
)