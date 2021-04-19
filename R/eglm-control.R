#' Auxiliary for Controlling GLM Fitting
#'
#' Auxiliary function for \code{eglm} fitting. Typically only used internally by
#'  \code{glm.wfit}, but may be used to construct a control argument to either
#'  function.
#'
#' @param epsilon positive convergence tolerance \eqn{\epsilon}; the iterations
#'  converge when \eqn{|dev - dev_{old}|/(|dev| + 0.1) < \epsilon}.
#' @param maxit integer giving the maximal number of IWLS iterations.
#' @param trace logical indicating if output should be produced for each
#'  iteration.
#'
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
