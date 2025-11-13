folder <- "C2010"
experimento <- "APO2010-202106"
user <- "vickydiliscia"

base_path <- file.path("/home", user, "dmeyf2025", "src", "churn", folder, "config")

# Lista de módulos a cargar
modules <- c("paths.R", "dataquality.R", "lgbm.R", "experiment.R", 
             "trainingstrategy.R", "features.R", "FE_rf.R", "FE_intrames.R")

# Cargar cada uno con control de existencia
for (mod in modules) {
  file_to_source <- file.path(base_path, mod)
  if (file.exists(file_to_source)) {
    source(file_to_source)
  } else {
    warning(sprintf("⚠️ No se encontró el archivo: %s", file_to_source))
  }
}
       
PARAM <- list()

PARAM$paths <- paths
PARAM$dataquality <- dataquality
PARAM$lgbm <- lgbm
PARAM$experiment <- experiment
PARAM$trainingstrategy <- trainingstrategy
PARAM$features <- features 
PARAM$user <- user
PARAM$experimento <- experimento
PARAM$folder <- folder
PARAM$FE_rf <- FE_rf
PARAM$FE_hist <- FE_hist
PARAM$FE_intrames <- FE_intrames

PARAM$semilla_primigenia <- 100271

# 🔹 Exportar al global env para uso directo
assign("PARAM", PARAM, envir = .GlobalEnv)
