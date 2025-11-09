# Sumar segura
suma_segura <- function(a, b) {
  ifelse(is.na(a) & is.na(b), NA_real_,
         rowSums(cbind(ifelse(is.na(a), 0, a),
                       ifelse(is.na(b), 0, b))))
}