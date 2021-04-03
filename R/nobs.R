#' @importFrom stats nobs
#' @export
#' @keywords internal
nobs.elm <- function(object, use.fallback = FALSE, ...) {
  if (!is.null(w <- object$weights)) sum(w != 0) else object$n
}

#' @export
#' @keywords internal
nobs.eglm <- nobs.elm
