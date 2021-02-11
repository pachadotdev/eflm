#' @export
#' @keywords internal
formula.elm <- function(x, ...) {
  form <- formula(x$terms)
  environment(form) <- environment(x$formula)
  form
}
