#' @export
#' @keywords internal
extractAIC.bglm <- function(fit, scale = 0, k = 2, ...) {
  n <- fit$n
  edf <- n - fit$df.null
  aic <- fit$aic
  c(edf, aic + (k - 2) * edf)
}
