#' @importFrom stats family
#' @export
fitted.fglm <- function(object, ...) {
  return(family(object)$linkinv(object$linear.predictors))
}
