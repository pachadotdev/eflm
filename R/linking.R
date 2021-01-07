#' @export
#' @keywords internal
family.fglm <- function(object, ...) {
  object$family
}

#' @export
#' @keywords internal
fitted.fglm <- function(object, ...) {
  return(family(object)$linkinv(object$linear.predictors))
}
