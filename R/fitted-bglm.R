#' @importFrom stats family
#' @export
#' @keywords internal
fitted.bglm <- function(object, ...) {
  return(family(object)$linkinv(object$linear.predictors))
}
