#' @importFrom stats update.default
#' @export
#' @keywords internal
update.blm <- function(object, formula, data, add = TRUE, evaluate = TRUE, ...) {
  if (!inherits(object, "blm")) {
    stop("object must be of class blm")
  }
  if ((!missing(formula)) & (!missing(data)) & (add)) {
    stop("cannot specify a formula while adding new data")
  }
  if ((!missing(data)) & (add)) {
    mod <- updateWithMoreData(object, data, formula = formula.blm(object), ...)
  }
  else {
    mod <- if (missing(data)) {
      update.default(object, formula, evaluate = evaluate, ...)
    } else {
      update.default(object, formula, data = data, evaluate = evaluate, ...)
    }
  }
  mod
}
