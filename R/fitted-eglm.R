#' @importFrom stats family
#' @export
#' @keywords internal
fitted.eglm <- function(object, ...) {
  return(family(object)$linkinv(object$linear.predictors))
}
