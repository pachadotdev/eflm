#' @export
#' @keywords internal
formula.blm <- function(x, ...) {
  form <- formula(x$terms)
  environment(form) <- environment(x$formula)
  form
}
