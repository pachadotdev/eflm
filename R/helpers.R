coefficient_names <- function(col.names, coefficients) {
  cn <- if (is.null(col.names) & (!is.null(coefficients))) {
    if (intercept) {
      if (length(coefficients) > 1) {
        c("(Intercept)", paste("V", 1:(length(coefficients) - 1), sep = ""))
      } else {
        "(Intercept)"
      }
    } else {
      paste("V", 1:length(coefficients), sep = "")
    }
  } else {
    col.names
  }

  return(cn)
}
