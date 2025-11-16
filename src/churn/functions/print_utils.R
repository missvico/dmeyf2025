# Opcional: imprimir en formato “lindo” con sufijo L
pretty_print_list <- function(lst) {
  lines <- mapply(function(nm, v) {
    paste0("  ", nm, " = c(", paste0(paste0(v, "L"), collapse = ", "), "),")
  }, names(lst), lst, SIMPLIFY = TRUE, USE.NAMES = FALSE)
  cat("list(\n", paste(lines, collapse = "\n"), "\n)\n", sep = "")
}
