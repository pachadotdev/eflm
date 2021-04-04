#' @importFrom stats update.default
#' @export
#' @keywords internal
update.elm <- function(object, formula, data, subset=NULL, add = TRUE, evaluate = TRUE,
                       offset = NULL, weights = NULL, ...) {
  if (!inherits(object, "elm")) {
    stop("object must be of class elm")
  }
  if ((!missing(formula)) & (!missing(data)) & (add)) {
    stop("cannot specify a formula while adding new data")
  }
  if ((!missing(data)) & (add)) {
    mod <- update_with_more_data(object, data, subset = subset,
                                 formula = formula.elm(object),
                                 offset = NULL, weights = NULL, ...)
  }
  else {
    mod <- if (missing(data)) {
      update.default(object, formula, evaluate = evaluate, subset = subset,
                     offset = NULL, weights = NULL, ...)
    } else {
      update.default(object, formula, data = data, evaluate = evaluate,
                     subset = subset, offset = NULL, weights = NULL, ...)
    }
  }
  mod
}
