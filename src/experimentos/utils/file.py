def read_polar(path):
    # Ruta al archivo — importante usar r"" para evitar problemas con backslashes en Windows
    #path = r"G:\My Drive\dmeyf\competencia_02_crudo.csv.gz"

    # Leer el CSV comprimido
    dataset = pl.read_csv(
        path,
        separator=",",            # usa ',' como separador
        has_header=True,    # primera fila son los nombres de columna
        infer_schema_length=5000,  # lee más filas para detectar bien los tipos
        ignore_errors=True, # salta filas corruptas si las hubiera
        low_memory=True     # usa menos RAM si el archivo es grande
    )

    #print(dataset.shape)
    #print(dataset.head())
    return dataset

def write_polar_gz(dataset,path_target):
    # === 7. Guardar resultado ===
    with gzip.open(path_target, "wt", encoding="utf-8") as f:
        dataset.write_csv(f)
    return