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
  y <- if (is.null(object$x)) {
    y2 <- as.matrix(model.frame(x))[,-1]
    if (any(names(object$coefficients) %in% "(Intercept)")) {
      cbind(
        matrix(1, nrow = nrow(y2), ncol = 1, dimnames = list(NULL, "(Intercept)")),
        y2
      )
    }
  } else {
    object$x
  }

  if (is.null(y)) {
    warning("The output is NULL because the model was specified with model=FALSE or the model component was removed from the regression object")
  }
  return(y)
}
