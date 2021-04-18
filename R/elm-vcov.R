#' @export
#' @noRd
vcov.elm <- function(object, complete = TRUE, ...) {
  vcov.summary.elm(summary.elm(object, ...), complete = complete)
}
