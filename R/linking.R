family.fglm <- function(object, ...) {
  object$family
}

#' @importFrom stats family
fitted.fglm <- function(object, ...) {
  return(family(object)$linkinv(object$linear.predictors))
}

#' @export
#' @keywords internal
model.matrix.fglm <- function (object, ...) {
  y <- object$model
  if (is.null(y)) {
    warning("The output is NULL because the model was specified with model=FALSE or the model component was removed from the regression object")
  }
  return(y)
}
