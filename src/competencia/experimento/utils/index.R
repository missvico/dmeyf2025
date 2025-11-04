# /home/vickydiliscia/dmeyf2025/src/competencia/utils/index.R

load_utils <- function(path, recursive = TRUE, quiet = TRUE) {
  if (missing(path) || is.null(path) || !nzchar(path)) {
    stop("Debes pasar 'path' con la carpeta utils, p.ej. load_utils('/home/.../utils')", call. = FALSE)
  }

  # listar .R (excluye este index.R)
  files <- list.files(
    path       = path,
    pattern    = "\\.R$",
    full.names = TRUE,
    recursive  = recursive
  )
  files <- files[basename(files) != "index.R"]
  files <- files[order(basename(files))]

  # source silencioso / verboso
  for (f in files) {
    if (!quiet) message("Sourcing: ", f)
    source(f, local = FALSE)
  }
  invisible(files)
}
