def generar_ternaria(dataset):
    # === 2. Calcular periodo0 consecutivo ===
    dataset = dataset.with_columns(
        ((pl.col("foto_mes") // 100) * 12 + (pl.col("foto_mes") % 100)).alias("periodo0")
    )

    # === 3. Calcular leads (mes siguiente y subsiguiente) por cliente ===
    dsimple = (
        dataset
        .sort(["numero_de_cliente", "periodo0"])
        .with_columns([
            pl.col("periodo0").shift(-1).over("numero_de_cliente").alias("periodo1"),
            pl.col("periodo0").shift(-2).over("numero_de_cliente").alias("periodo2"),
        ])
    )

    # === 4. Calcular topes ===
    periodo_ultimo = dsimple["periodo0"].max()
    periodo_anteultimo = periodo_ultimo - 1

    # === 5. Crear columna clase_ternaria ===
    #   Regla base: CONTINUA
    #   Luego se sobrescriben BAJA+1 y BAJA+2

    dsimple = dsimple.with_columns(pl.lit(None).alias("clase_ternaria"))

    dsimple = dsimple.with_columns(
        pl.when(pl.col("periodo0") < periodo_anteultimo)
        .then(pl.lit("CONTINUA"))              # ← pl.lit() asegura que es un valor literal
        .otherwise(pl.col("clase_ternaria"))
        .alias("clase_ternaria")
    )

    dsimple = dsimple.with_columns(
        pl.when(
            (pl.col("periodo0") < periodo_ultimo) &
            ((pl.col("periodo1").is_null()) | (pl.col("periodo0") + 1 < pl.col("periodo1")))
        )
        .then(pl.lit("BAJA+1"))                   # ← ¡usa pl.lit()!
        .otherwise(pl.col("clase_ternaria"))
        .alias("clase_ternaria")
    )

    dsimple = dsimple.with_columns(
        pl.when(
            (pl.col("periodo0") < periodo_anteultimo) &
            (pl.col("periodo0") + 1 == pl.col("periodo1")) &
            ((pl.col("periodo2").is_null()) | (pl.col("periodo0") + 2 < pl.col("periodo2")))
        )
        .then(pl.lit("BAJA+2"))                     # ← usar pl.lit() siempre
        .otherwise(pl.col("clase_ternaria"))
        .alias("clase_ternaria")
    )

    # === 6. Reinsertar en dataset original ===
    dataset = (
        dataset.join(
            dsimple.select(["numero_de_cliente", "periodo0", "clase_ternaria"]),
            on=["numero_de_cliente", "periodo0"],
            how="left"
        )
    )
    return dataset

def generar_columna_target(dataset):
    # target = 1 si clase_ternaria ∈ ["BAJA+1", "BAJA+2"], sino 0
    dataset = dataset.with_columns([
        pl.col("clase_ternaria")
          .is_in(["BAJA+1", "BAJA+2"])
          .cast(pl.Int8)
          .alias("target"),
    
        # clase_peso = 1.0
        pl.lit(1.0).alias("clase_peso")
    ])
    return dataset

def generar_delta_lags(dataset):
    # Ordenar por cliente y mes (igual que sort_values)
    dataset = dataset.sort(["numero_de_cliente", "foto_mes"])
    
    exclude_cols = {"numero_de_cliente", "foto_mes", "clase_ternaria", "target", "clase_peso"}
    
    numeric_types = (
        pl.Int8, pl.Int16, pl.Int32, pl.Int64,
        pl.UInt8, pl.UInt16, pl.UInt32, pl.UInt64,
        pl.Float32, pl.Float64
    )
    
    fe_cols = [
        c for c, dtype in zip(dataset.columns, dataset.dtypes)
        if c not in exclude_cols and isinstance(dtype, numeric_types)
    ]
    
    #print(f"Columnas numéricas seleccionadas: {len(fe_cols)}")
    
    # Paso 1: crear los lagN
    for n in [1, 2]:
        lag_exprs = [
            pl.col(c).shift(n).over("numero_de_cliente").alias(f"{c}_lag{n}")
            for c in fe_cols
        ]
        dataset = dataset.with_columns(lag_exprs)
    
    # Paso 2: crear los dlagN (una vez que ya existen los lagN)
    for n in [1, 2]:
        dlag_exprs = [
            (pl.col(c) - pl.col(f"{c}_lag{n}")).alias(f"{c}_dlag{n}")
            for c in fe_cols
        ]
        dataset = dataset.with_columns(dlag_exprs)
    
    print(f"Lags/deltas agregados: {len(fe_cols)*2}")
    return dataset

def convertir_percentil(dataset):
    # --- 1️⃣ Ordenar por fecha y cliente
    dataset = dataset.sort(["foto_mes", "numero_de_cliente"])
    
    # --- 2️⃣ Detectar columnas numéricas (excepto las de control)
    exclude_cols = {"numero_de_cliente", "foto_mes", "clase_ternaria", "target", "clase_peso"}
    
    numeric_types = (
        pl.Int8, pl.Int16, pl.Int32, pl.Int64,
        pl.UInt8, pl.UInt16, pl.UInt32, pl.UInt64,
        pl.Float32, pl.Float64
    )
    
    num_cols = [
        c for c, dtype in dataset.schema.items()
        if c not in exclude_cols and dtype in numeric_types
    ]
    
    #print(f"Columnas numéricas detectadas: {num_cols}")
    
    # --- 3️⃣ Función auxiliar: percentil dentro del grupo de foto_mes
    def to_percentile_expr(col):
        # rank() / count() * 100 → percentil [0,100]
        return (pl.col(col).rank("average") / pl.count() * 100).cast(pl.Float32)
    
    # --- 4️⃣ Aplicar transformación por grupo de foto_mes
    for c in num_cols:
        new_col = f"{c}_pctl"
        dataset = (
            dataset.with_columns(
                to_percentile_expr(c).over("foto_mes").alias(new_col)
            )
            .drop(c)  # quitar la original; si querés mantenerla, eliminá esta línea
        )
        #print(f"✅ Convertida {c} → {new_col}")
    
    print(f"\nTotal columnas convertidas: {len(num_cols)}")
    return dataset


__all__ = ["generar_ternaria", "generar_columna_target", "generar_delta_lags", "convertir_percentil"]
