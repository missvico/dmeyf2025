paths <- list()

# Paths Base
paths$experimento <- "APO2010-202106""
paths$scriptfolder <- "C2010"
paths$user <- "vickydiliscia"
paths$base <- paste0("/home/", paths$user, "/dmeyf2025")
paths$bucket <- paste0("/home/", paths$user, "/buckets/b1")
paths$experiments     <- paste0(paths$bucket, "/exp")

paths$churn     <- paste0(paths$base, "/src/churn")
paths$script <- paste0(paths$churn, "/", paths$scriptfolder)

# Paths Input
paths$datasets   <- paste0(paths$bucket, "/datasets")
paths$datasetcrudo    <- paste0(paths$datasets , "/competencia_02_crudo.csv.gz")
paths$datasetclases <- paste0(paths$datasets , "/competencia_02.csv.gz")

# Paths codigos
paths$churn     <- paste0(paths$base, "/src/churn")
paths$functions <- paste0(paths$churn, "/functions")
paths$script <- paste0(paths$churn, "/", paths$scriptfolder)

# Resultados
paths$outputs    <- paste0(paths$bucket, "/outputs")
paths$results     <- paste0(paths$experiments, "/", paths$experimento )
paths$optimization <- paste0(paths$results, "/optimization")
paths$optimization <- paste0(paths$results , "/models")
paths$prediction <- paste0(paths$results , "/prediction")
paths$kaggle <- paste0(paths$results , "/kaggle")
paths$backtesting <- paste0(paths$results , "/backtesting")
