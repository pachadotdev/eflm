# sandwich ----

bread.eglm <- function(x, ...) {
  if (!is.null(x$na.action)) class(x$na.action) <- "omit"
  sx <- summary(x)
  wres <- as.vector(residuals(x, "working")) * weights(x, "working")
  dispersion <- if (x$family$family %in% c("poisson", "binomial")) {
    1
  } else {
    sum(wres^2) / sum(weights(x, "working"))
  }
  return(sx$cov.unscaled * as.vector(sum(sx$df[1L:2L])) * dispersion)
}

bread.elm <- function(x, ...) {
  if(!is.null(x$na.action)) class(x$na.action) <- "omit"
  sx <- summary(x)
  return(sx$cov.unscaled * as.vector(sum(sx$df[1L:2L])))
}
