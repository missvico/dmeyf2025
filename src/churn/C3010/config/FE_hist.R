util <- list()

util$user <- "vickydiliscia"
util$scriptfolder <- "C3010"
util$base <- paste0("/home/", paths$user, "/dmeyf2025")
util$churn <- paste0(paths$base, "/src/churn")

FE_hist<- list()

FE_hist$Ipc$Run <- TRUE
FE_hist$Lags$Run <- TRUE
FE_hist$Lags$Order <- c(1,2,3)

FE_hist$DeltaLags$Run <- TRUE
FE_hist$DeltaLags$Order <- c(1,2,3)
FE_hist$spechistoricas <- paste0(util$churn, "/", util$scriptfolder, "/feature_specs/spec_historicas.txt")

FE_hist$specsipc <- paste0(util$churn, "/", util$scriptfolder, "/feature_specs/ipc.txt")

# parametros de Feature Engineering Historico de Tendencias
FE_hist$Tendencias$run <- TRUE
FE_hist$Tendencias$ventana <- 6
FE_hist$Tendencias$tendencia <- TRUE
FE_hist$Tendencias$minimo <- FALSE
FE_hist$Tendencias$maximo <- FALSE
FE_hist$Tendencias$promedio <- FALSE
FE_hist$Tendencias$ratioavg <- FALSE
FE_hist$Tendencias$ratiomax <- FALSE