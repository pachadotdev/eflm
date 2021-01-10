#' @export
#' @keywords internal
AIC.fglm <- function(object, ...) {
  if (!(length(list(...)))) {
    object$aic
  } else {
    aic <- function(x) x$aic
    object <- list(object, ...)
    val <- sapply(object, aic)
    val
  }
}

extractAIC.fglm <- function(fit, scale = 0, k = 2, ...) {
  n <- fit$n
  edf <- n - fit$df.null
  aic <- fit$aic
  c(edf, aic + (k - 2) * edf)
}
