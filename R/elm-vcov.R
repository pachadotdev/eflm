#' @export
#' @noRd
vcov.elm <- function(object, complete = TRUE, ...) {
  return(vcov.summary.elm(summary(object, ...), complete = complete))
}
