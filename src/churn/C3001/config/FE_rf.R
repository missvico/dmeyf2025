FE_rf <- list()

# Estos CUATRO parametros son los que se deben modificar
FE_rf$arbolitos= 20
FE_rf$hojas_por_arbol= 16
FE_rf$datos_por_hoja= 100
FE_rf$mtry_ratio= 0.2

# Estos son quasi fijos
FE_rf$train$training <- c( 202101, 202102, 202103)

# Estos TAMBIEN son quasi fijos
FE_rf$lgb_param <-list(
    # parametros que se pueden cambiar
    num_iterations = FE_rf$arbolitos,
    num_leaves  = FE_rf$hojas_por_arbol,
    min_data_in_leaf = FE_rf$datos_por_hoja,
    feature_fraction_bynode  = FE_rf$mtry_ratio,

    # para que LightGBM emule Random Forest
    boosting = "rf",
    bagging_fraction = ( 1.0 - 1.0/exp(1.0) ),
    bagging_freq = 1.0,
    feature_fraction = 1.0,

    # genericos de LightGBM
    max_bin = 31L,
    objective = "binary",
    first_metric_only = TRUE,
    boost_from_average = TRUE,
    feature_pre_filter = FALSE,
    force_row_wise = TRUE,
    verbosity = -100,
    max_depth = -1L,
    min_gain_to_split = 0.0,
    min_sum_hessian_in_leaf = 0.001,
    lambda_l1 = 0.0,
    lambda_l2 = 0.0,

    pos_bagging_fraction = 1.0,
    neg_bagging_fraction = 1.0,
    is_unbalance = FALSE,
    scale_pos_weight = 1.0,

    drop_rate = 0.1,
    max_drop = 50,
    skip_drop = 0.5,

    extra_trees = FALSE
  )