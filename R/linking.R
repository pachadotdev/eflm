family.fglm <- function(object, ...) {
  object$family
}

#' @importFrom stats family
fitted.fglm <- function(object, ...) {
  return(stats::family(object)$linkinv(object$linear.predictors))
}
