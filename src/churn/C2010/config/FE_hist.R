FE_hist<- list()

FE_hist$Lags$run <- TRUE
FE_hist$Lags$Order <- c(1,2)

FE_hist$DeltaLags$run <- TRUE
FE_hist$DeltaLags$Order <- c(1,2)

# parametros de Feature Engineering Historico de Tendencias
FE_hist$Tendencias$run <- TRUE
FE_hist$Tendencias$ventana <- 6
FE_hist$Tendencias$tendencia <- TRUE
FE_hist$Tendencias$minimo <- FALSE
FE_hist$Tendencias$maximo <- FALSE
FE_hist$Tendencias$promedio <- FALSE
FE_hist$Tendencias$ratioavg <- FALSE
FE_hist$Tendencias$ratiomax <- FALSE