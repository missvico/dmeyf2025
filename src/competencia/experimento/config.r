# Configuración global del proyecto

# Usuario actual (por si cambia el entorno)
USER <- "vickydiliscia"

# Paths principales
PATH_BASE       <- paste0("/home/", USER, "/dmeyf2025")
PATH_BUCKET     <- paste0("/home/", USER, "/buckets/b1")
PATH_DATASETS   <- paste0(PATH_BUCKET, "/datasets")
PATH_OUTPUTS    <- paste0(PATH_BUCKET, "/outputs")
PATH_EXPS <- paste0(PATH_BUCKET, "/exp")
PATH_EXPERIMENT <- paste0(PATH_BASE, "/src/competencia/experimento")
PATH_UTILS <- paste0(PATH_EXPERIMENT, "/utils/index.R")

# Variables de proyecto
DATASET_CRUDO   <- paste0(PATH_DATASETS, "/competencia_01_crudo.csv")
DATASET_CLASE <- paste0(PATH_DATASETS, "/competencia_01.csv.gz")
DATASET_FEATURES <- paste0(PATH_DATASETS, "/competencia_01_features.csv.gz")

# Parámetros del modelo
SEEDS            <- 42
N_CLUSTERS      <- 5
SEEDS <- c(
  100271,
  250061,
  345803,
  567983,
  890009,
  910037,
  970019,
  990137,
  860689,
  780079
)