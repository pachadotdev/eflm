ll.bglm <- function(family, aic.model, nvar) {
  switch(family,
    binomial = -(aic.model - 2 * nvar) / 2,
    Gamma = -((aic.model - 2 * nvar) - 2) / 2,
    gaussian = -((aic.model - 2 * nvar) - 2) / 2,
    poisson = -(aic.model - 2 * nvar) / 2,
    inverse.gaussian = -((aic.model - 2 * nvar) - 2) / 2,
    quasi = NA,
    quasibinomial = NA,
    quasipoisson = NA
  )
}

#' @importFrom stats family
#' @export
#' @keywords internal
logLik.bglm <- function(object, ...) {
  if (!missing(...)) {
    warning("extra arguments discarded")
  }
  fam <- stats::family(object)$family
  p <- object$rank
  if (fam %in% c("gaussian", "Gamma", "inverse.gaussian")) {
    p <- p + 1
  }
  val <- p - object$aic / 2
  attr(val, "nobs") <- object$n
  attr(val, "df") <- p
  class(val) <- "logLik"
  val
}
