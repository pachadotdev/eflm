#' @export
#' @noRd
vcov.eglm <- function(object, complete = TRUE, ...) {
  return(vcov.summary.eglm(summary(object, ...), complete = complete))
}
