this_dir <- dirname(normalizePath(sys.frame(1)$ofile))
setwd(this_dir)

source("paths.R")
source("dataquality.R")
source("lgbm.R")
source("experiment.R")
source("trainingstrategy.R")
source("features.R")

# 🔹 Función auxiliar para merge recursivo
deep_merge <- function(x, y) {
  if (is.null(x)) return(y)
  if (is.null(y)) return(x)
  if (is.list(x) && is.list(y)) {
    for (n in names(y)) x[[n]] <- deep_merge(x[[n]], y[[n]])
    return(x)
  }
  y
}

PARAM <- list()
for (obj in c("paths", "dataquality", "lgbm", "experiment", "trainingstrategy")) {
  if (exists(obj)) {
    PARAM <- deep_merge(PARAM, get(obj))
  } else {
    warning(sprintf("⚠️ No se encontró el objeto %s, se omite.", obj))
  }
}

# Variables base
PARAM$user <- "vickydiliscia"
PARAM$experimento <- "APO2010-202106"
PARAM$semilla_primigenia <- 100271

# 🔹 Exportar al global env para uso directo
assign("PARAM", PARAM, envir = .GlobalEnv)
