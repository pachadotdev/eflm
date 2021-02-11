#' @export
#' @keywords internal
vcov.eglm <- function(object, ...) {
  object$dispersion * solve(object$XTX)
}
