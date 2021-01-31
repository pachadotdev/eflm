nobs.bglm <- function(object, use.fallback = FALSE, ...) {
  if (!is.null(w <- object$weights)) sum(w != 0) else object$n
}
