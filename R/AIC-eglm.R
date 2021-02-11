#' @export
#' @keywords internal
AIC.eglm <- function(object, ...) {
  if (!(length(list(...)))) {
    object$aic
  } else {
    aic <- function(x) x$aic
    object <- list(object, ...)
    val <- sapply(object, aic)
    val
  }
}
