def undersampling_experimento(dataset,undersampling,train_months,seed):
    # --- parámetros de entrenamiento ---  
    dataset = dataset.filter(pl.col("foto_mes").is_in(train_months))

    # --- generar la columna azar reproducible ---
    rng = np.random.default_rng(seed)
    dataset = dataset.with_columns(
        pl.Series("azar", rng.random(len(dataset)))  # uniforme entre 0 y 1
    )
    
    # --- inicializar columna training ---
    dataset = dataset.with_columns(pl.lit(0).alias("training"))
    
    # --- aplicar la misma condición que en R ---
    dataset = dataset.with_columns(
        pl.when(
                (pl.col("azar") <= undersampling) |
                (pl.col("clase_ternaria").is_in(["BAJA+1", "BAJA+2"]))
        )
        .then(1)
        .otherwise(0)
        .alias("training")
    )
    
    # --- chequeo ---
    print(dataset.select(pl.col("training")).to_series().value_counts())
    print(dataset.group_by(["training", "clase_ternaria"]).len().sort("training"))
    return dataset