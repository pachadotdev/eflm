#' @importFrom stats gaussian na.pass
#' @export
eglm.control <- function(epsilon = 1e-8, maxit = 25, trace = FALSE) {
  if (!is.numeric(epsilon) || epsilon <= 0) {
    stop("value of 'epsilon' must be > 0")
  }
  if (!is.numeric(maxit) || maxit <= 0) {
    stop("maximum number of iterations must be > 0")
  }
  list(epsilon = epsilon, maxit = maxit, trace = trace)
}
