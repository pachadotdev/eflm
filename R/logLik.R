#' @importFrom stats family
#' @export
#' @keywords internal
logLik.eglm <- function(object, ...) {
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
