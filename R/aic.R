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

#' @export
#' @keywords internal
extractAIC.fglm <- function(fit, scale = 0, k = 2, ...) {
  n <- fit$n
  edf <- n - fit$df.null
  aic <- fit$aic
  c(edf, aic + (k - 2) * edf)
}

aic.Gamma <- function(y, n, mu, wt, dev) {
  disp <- dev / n
  -2 * sum(dgamma(y, 1 / disp, scale = mu * disp, log = TRUE) * wt) #+2
}

aic.binomial <- function(y, n, mu, wt, dev) {
  m <- if (any(n > 1)) n else wt
  -2 * sum(ifelse(m > 0, (wt / m), 0) * dbinom(round(m * y),
                                               round(m), mu,
                                               log = TRUE
  ))
}

aic.inverse.gaussian <- function(y, n, mu, wt, dev) {
  sum(wt) * (log(dev / sum(wt) * 2 * pi) + 1) + 3 * sum(log(y) * wt)
}

aic.poisson <- function(y, n, mu, wt, dev) {
  -2 * sum(dpois(y, mu, log = TRUE) * wt)
}

aic.gaussian <- function(y, n, mu, wt, dev) {
  n.obs <- length(y)
  n.obs * (log(dev / n.obs * 2 * pi) + 1) - sum(log(wt))
}

aic.shglm <- function(family, y, n, mu, wt, dev) {
  options(warn = -1)
  a <- switch(family,
              binomial = aic.binomial(y, n, mu, wt, dev),
              Gamma = aic.Gamma(y, n, mu, wt, dev),
              gaussian = aic.gaussian(y, n, mu, wt, dev),
              poisson = aic.poisson(y, n, mu, wt, dev),
              inverse.gaussian = aic.inverse.gaussian(y, n, mu, wt, dev),
              quasi = NA,
              quasibinomial = NA,
              quasipoisson = NA
  )
  options(warn = 1)
  return(a)
}
