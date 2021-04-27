#' @export
#' @noRd
vcov.elm <- function(object, complete = TRUE, ...) {
  vcov.summary.elm(summary(object, ...), complete = complete)
}
