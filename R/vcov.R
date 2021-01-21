#' @export
vcov.fglm <- function(object, ...) {
  object$dispersion * solve(object$XTX)
}
