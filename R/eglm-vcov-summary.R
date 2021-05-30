#' @export
#' @noRd
vcov.summary.eglm <- function(object, complete = TRUE, ...) {
  return(.vcov.aliased(object$aliased, object$cov.scaled, complete = complete))
}
