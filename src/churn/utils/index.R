# index.R
# Carga automática de todos los scripts de utils salvo este mismo archivo

source_utils <- function(path) {
  utils_files <- list.files(
    path = path,
    pattern = "\\.R$",
    full.names = TRUE
  )
  utils_files <- utils_files[!grepl("index\\.R$", utils_files)]

  for (f in utils_files) {
    message(sprintf("Sourcing: %s", basename(f)))
    tryCatch(
      source(f, encoding = "UTF-8"),
      error = function(e) {
        message(sprintf("❌ Error al cargar %s: %s", basename(f), e$message))
      }
    )
  }
  message("✅ Todos los utils fueron cargados correctamente.")
}

