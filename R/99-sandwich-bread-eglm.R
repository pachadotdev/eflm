# Dynamically exported, see zzz.R

bread.eglm <- function(x, ...) {
  if (!is.null(x$na.action)) class(x$na.action) <- "omit"
  sx <- summary(x)
  wres <- as.vector(residuals(x, "working")) * weights(x, "working")
  dispersion <- if (substr(x$family$family, 1L, 17L) %in%
    c("poisson", "binomial", "Negative Binomial")) {
    1
  } else {
    sum(wres^2) / sum(weights(x, "working"))
  }
  return(sx$cov.unscaled * as.vector(sum(sx$df[1L:2L])) *
    dispersion)
}
