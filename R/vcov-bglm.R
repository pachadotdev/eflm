#' @export
#' @keywords internal
vcov.bglm <- function(object, ...) {
  object$dispersion * solve(object$XTX)
}
