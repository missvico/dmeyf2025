# =====================================
# Configuración global del proyecto
# =====================================

# Usuario actual
USER = "vickydiliscia"

# Experimento
EXPERIMENT = "6001"
EXPERIMENT_FOLDER = f"EXP{EXPERIMENT}"
UNDERSAMPLING = 0.1
LAGS = 2

# Tag de experimento
us_pct = round(UNDERSAMPLING * 100)  # 0.1 -> 10
TAG = f"{EXPERIMENT}_lag{LAGS}_us{us_pct:02d}"

# =====================================
# Paths principales
# =====================================

PATH_BASE = f"/home/{USER}/dmeyf2025"
PATH_BUCKET = f"/home/{USER}/buckets/b1"
PATH_DATASETS = f"{PATH_BUCKET}/datasets"
PATH_OUTPUTS = f"{PATH_BUCKET}/outputs"
PATH_EXPERIMENTS = f"{PATH_BUCKET}/exp"

PATH_CHURN = f"{PATH_BASE}/src/churn"
PATH_UTILS = f"{PATH_CHURN}/utils"

PATH_EXP_CODE = f"{PATH_BASE}/src/churn{EXPERIMENT}"
PATH_THIS_EXPERIMENT = f"{PATH_EXPERIMENTS}/{EXPERIMENT_FOLDER}"

# =====================================
# Resultados
# =====================================

PATH_BO = f"{PATH_THIS_EXPERIMENT}/optimization"
PATH_MODELS = f"{PATH_THIS_EXPERIMENT}/models"
PATH_TEST_MODELS = f"{PATH_THIS_EXPERIMENT}/test_models"
PATH_PREDICTION = f"{PATH_THIS_EXPERIMENT}/prediction"
PATH_TEST_PREDICTION = f"{PATH_THIS_EXPERIMENT}/test_prediction"
PATH_KAGGLE = f"{PATH_THIS_EXPERIMENT}/kaggle"

# =====================================
# Datasets
# =====================================

DATASET_CRUDO = f"{PATH_DATASETS}/competencia_02_crudo.csv"
DATASET_CLASE = f"{PATH_DATASETS}/competencia_01.csv.gz"
DATASET_FEATURES = f"{PATH_DATASETS}/competencia_01_lg2.csv.gz"

# =====================================
# Parámetros del modelo
# =====================================

TRAIN_MONTHS = [202101, 202102, 202103,202104,202105]
N_CLUSTERS = 5
SEEDS = [
    100271,
    250061,
    345803,
    567983,
    890009,
    910037,
    970019,
    990137,
    860689,
    780079,
]
