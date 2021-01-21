#' @export
#' @keywords internal
nobs.fglm <- function(object, ...) {
  if (!is.null(w <- object$weights)) sum(w != 0) else object$n
}
