#' @export
#' @keywords internal
model.matrix.eglm <- function (object, ...) {
  y <- if (is.null(object$x)) {
    obtain_model_matrix <- function(model, data) {
      call <- match.call()
      M <- match.call(expand.dots = FALSE)
      m <- match(c("formula", "data"), names(M), 0L)
      M <- M[c(1L, m)]
      M$drop.unused.levels <- TRUE
      M[[1L]] <- quote(stats::model.frame)
      M <- eval(M, parent.frame())
      tf <- attr(M, "terms")
      X <- model.matrix(tf, M)
      X <- X[, colnames(X) %in% names(coef(object))]
      attr(X, "assign") <- seq_along(colnames(X)) - 1
      X
    }

    obtain_model_matrix(model = object$call$formula,
                        data = eval(object$call$data))
  } else {
    object$x
  }

  if (is.null(y)) {
    warning("The output is NULL because the model was specified with model=FALSE or the model component was removed from the regression object")
  }
  return(y)
}
