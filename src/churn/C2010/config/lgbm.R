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
  min_data_in_leaf= 20L,  #este ya es el valor default de LightGBM

  num_iterations= 9999L, # dejo libre la cantidad de arboles, zLightGBM se detiene solo
  num_leaves= 999L, # dejo libre la cantidad de hojas, zLightGBM sabe cuando no hacer un split
  learning_rate= 1.0,  # se lo deja en 1.0 para que si el score esta por debajo de gradient_bound no se lo escale
    
  feature_fraction= 0.50, # un valor equilibrado, habra que probar alternativas ...
    
  canaritos= 100, # fundamental en zLightGBM, aqui esta el control del overfitting
  gradient_bound= 0.1  # default de zLightGBM
)