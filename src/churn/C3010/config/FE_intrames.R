util <- list()

util$user <- "vickydiliscia"
util$scriptfolder <- "C3010"
util$base <- paste0("/home/", paths$user, "/dmeyf2025")
util$churn <- paste0(paths$base, "/src/churn")

FE_intrames <- list()

FE_intrames$kmes <- TRUE
FE_intrames$normquarter <- TRUE
FE_intrames$specsumas <- paste0(util$churn, "/", util$scriptfolder, "/feature_specs/spec_sumas.txt")
FE_intrames$ratio_payroll_edad <- TRUE