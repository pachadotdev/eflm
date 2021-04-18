#' @export
#' @noRd
vcov.eglm <- function(object, complete = TRUE, ...) {
  vcov.summary.eglm(summary.eglm(object, ...), complete=complete)
}
