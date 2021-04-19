#' @importFrom stats .vcov.aliased
#' @export
#' @noRd
vcov.summary.eglm <- function(object, complete = TRUE, ...) {
  .vcov.aliased(object$aliased, object$cov.scaled, complete = complete)
}
