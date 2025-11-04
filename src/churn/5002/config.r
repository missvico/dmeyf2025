# Configuración global del proyecto

# Usuario actual (por si cambia el entorno)
USER <- "vickydiliscia"

# Usuario actual (por si cambia el entorno)
EXPERIMENT <- "5002"
EXPERIMENT_FOLDER  <- "EXP5002" 
UNDERSAMPLING <- 0.1
LAGS <- 2

EXPERIMENT_FOLDER  <- paste0("EXP", EXPERIMENT)
us_pct <- round(UNDERSAMPLING * 100)   # 0.1 -> 10
TAG    <- sprintf("%s_lag%d_us%02d", EXPERIMENT, LAGS, us_pct)

# Paths principales
PATH_BASE       <- paste0("/home/", USER, "/dmeyf2025")
PATH_BUCKET     <- paste0("/home/", USER, "/buckets/b1")
PATH_DATASETS   <- paste0(PATH_BUCKET, "/datasets")
PATH_OUTPUTS    <- paste0(PATH_BUCKET, "/outputs")
PATH_EXPERIMENTS    <- paste0(PATH_BUCKET, "/exp")

PATH_CHURN <- paste0(PATH_BASE, "/src/churn")
PATH_UTILS <- paste0(PATH_CHURN, "/utils")

PATH_EXP_CODE <- paste0(PATH_BASE, "/src/churn", EXPERIMENT)
PATH_THIS_EXPERIMENT <- paste0(PATH_EXPERIMENTS, "/", EXPERIMENT_FOLDER)

# Resultados
PATH_BO <- paste0(PATH_THIS_EXPERIMENT, "/optimization")
PATH_MODELS <- paste0(PATH_THIS_EXPERIMENT, "/models")
PATH_TEST_MODELS <- paste0(PATH_THIS_EXPERIMENT, "/test_models")
PATH_PREDICTION <- paste0(PATH_THIS_EXPERIMENT, "/prediction")
PATH_TEST_PREDICTION <- paste0(PATH_THIS_EXPERIMENT, "/test_prediction")
PATH_KAGGLE <- paste0(PATH_THIS_EXPERIMENT, "/kaggle")


# Variables de base de datos
DATASET_CRUDO   <- paste0(PATH_DATASETS, "/competencia_01_crudo.csv")
DATASET_CLASE <- paste0(PATH_DATASETS, "/competencia_01.csv.gz")
DATASET_FEATURES <- paste0(PATH_DATASETS, "/competencia_01_lg2.csv.gz")

# Parámetros del modelo
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