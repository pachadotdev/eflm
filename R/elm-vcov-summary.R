#' @export
#' @noRd
vcov.summary.elm <- function(object, complete = TRUE, ...) {
  return(.vcov.aliased(object$aliased, object$sigma^2 * object$cov.unscaled, complete = complete))
}
