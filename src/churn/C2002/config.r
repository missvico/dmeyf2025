# Configuración global del proyecto
PARAM <- list()

# Usuario actual (por si cambia el entorno)
PARAM$user <- "vickydiliscia"

# Usuario actual (por si cambia el entorno)
PARAM$experimento <- "APO2002-202106"
PARAM$scriptfolder <- "C2002"

# Configuracion 
PARAM$semilla_primigenia <- 100271

# Training Strategy
PARAM$features$lags <- 2

# Training Strategy
PARAM$trainingstrategy$testing <- c(202106)
PARAM$trainingstrategy$training <- c(
  201901, 201902, 201903, 201904, 201905, 201906,
  201907, 201908, 201909, 201910, 201911, 201912,
  202001, 202002, 202003, 202004, 202005, 202006,
  202007, 202008, 202009, 202010, 202011, 202012,
  202101, 202102, 202103, 202104
)

PARAM$trainingstrategy$undersampling <- 0.05
PARAM$trainingstrategy$positivos <- c( "BAJA+1", "BAJA+2")

# Paths principales
PARAM$paths$base <- paste0("/home/", PARAM$user, "/dmeyf2025")
PARAM$paths$bucket <- paste0("/home/", PARAM$user, "/buckets/b1")
PARAM$paths$datasets   <- paste0(PARAM$paths$bucket, "/datasets")
PARAM$paths$outputs    <- paste0(PARAM$paths$bucket, "/outputs")
PARAM$paths$experiments     <- paste0(PARAM$paths$bucket, "/exp")

PARAM$paths$churn     <- paste0(PARAM$paths$base, "/src/churn")
PARAM$paths$functions <- paste0(PARAM$paths$churn, "/functions")

PARAM$paths$script <- paste0(PARAM$paths$churn, "/", PARAM$scriptfolder)
PARAM$utils$promedios <- paste0(PARAM$paths$script, "/", "promedios.R")
PARAM$utils$ratios <- paste0(PARAM$paths$script, "/", "ratios.R")

# Resultados
PARAM$paths$results     <- paste0(PARAM$paths$experiments, "/", PARAM$experimento )
PARAM$paths$optimization <- paste0(PARAM$paths$results, "/optimization")
PARAM$paths$optimization <- paste0(PARAM$paths$results , "/models")
PARAM$paths$prediction <- paste0(PARAM$paths$results , "/prediction")
PARAM$paths$kaggle <- paste0(PARAM$paths$results , "/kaggle")
PARAM$paths$backtesting <- paste0(PARAM$paths$results , "/backtesting")

# Variables de base de datos
PARAM$paths$datasetcrudo    <- paste0(PARAM$paths$datasets , "/competencia_02_crudo.csv.gz")
PARAM$paths$datasetclases <- paste0(PARAM$paths$datasets , "/competencia_02.csv.gz")


# Parámetros del modelo
PARAM$qcanaritos      <- 100
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