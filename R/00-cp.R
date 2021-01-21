#' @importFrom methods as
cp <- function(X, w = NULL) {
  new.B <- if (is.null(w)) {
    crossprod(X)
  } else {
    crossprod(sqrt(w) * X)
  }
  as(new.B, "matrix")
}
