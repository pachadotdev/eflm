# Dynamically exported, see zzz.R
#' @importFrom stats residuals weights is.ts ts start frequency
#' @importFrom zoo is.zoo zoo index
estfun.fglm <- function(x, ...) {
  xmat <- model.matrix(x)
  xmat <- naresid(x$na.action, xmat)
  if (any(alias <- is.na(coef(x)))) xmat <- xmat[, !alias, drop = FALSE]
  wres <- as.vector(residuals(x, "working")) * weights(x, "working")
  dispersion <- if (substr(x$family$family, 1, 17) %in% c("poisson", "binomial", "Negative Binomial")) {
    1
  } else {
    sum(wres^2, na.rm = TRUE) / sum(weights(x, "working"), na.rm = TRUE)
  }
  rval <- wres * xmat / dispersion
  attr(rval, "assign") <- NULL
  attr(rval, "contrasts") <- NULL
  res <- residuals(x, type = "pearson")
  if (is.ts(res)) rval <- ts(rval, start = start(res), frequency = frequency(res))
  if (is.zoo(res)) rval <- zoo(rval, index(res), attr(res, "frequency"))
  return(rval)
}
