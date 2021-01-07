#' Fitting Generalized Linear Models
#'
#' fglm is used to fit generalized linear models, specified by giving a symbolic
#' description of the linear predictor and a description of the error
#' distribution.
#'
#' @param formula A formula for the model
#' @param data A tibble or data.frame
#' @param family See the function \link{glm}, but here it must be specified with brackets (e.g. \code{quasipoisson()})
#' @param intercept Logical value to determine whearead to included an intercept in the null model (Defaults to \code{TRUE})
#' @param weights An optional vector of ‘prior weights’ to be used in the fitting process. Should be \code{NULL}
#' or a numeric vector (e.g. \code{data$weights}, defaults to \code{NULL})
#' @param start Starting values for the parameters in the linear predictor
#' @param etastart Starting values for the linear predictor
#' @param mustart Starting values for the vector of means
#' @param offset This can be used to specify an a priori known component to be included in the
#' linear predictor during fitting. This should be \code{NULL} or a numeric vector of length
#' equal to the number of cases. One or more offset terms can be included in the formula instead
#' or as well, and if more than one is specified their sum is used. See \code{stats::model.offset()}
#' @param maxit See the function \link{glm}
#' @param k The penalty per parameter to be used, the default \code{k = 2}
#' is the classical AIC
#' @param model A logical value indicating whether model frame should be included as a
#' component of the returned value (Defaults to \code{TRUE})
#' @param method The chosen method to detect for singularity. Defaults to \code{"eigen"} but
#' it can also be \code{"Cholesky"} or \code{"qr"}
#' @param x Logical value indicating whether the model matrix used in the fitting process
#' should be returned as components of the returned value (Defaults to \code{FALSE}, see the function \link{glm.fit})
#' @param y Logical value indicating whether the response vector used in the fitting process
#' should be returned as components of the returned value (Defaults to \code{FALSE}, see the function \link{glm.fit})
#' @param tol.estimation Tolerance to be used for the estimation (Defaults to 1e-8)
#' @param tol.solve (Defaults to \code{.Machine$double.eps}, see the function \link{solve()})
#' @param tol.values Tolerance to consider eigenvalues equal to zero (Defaults to 1e-7, see the function \link{control()}),
#' @param tol.vectors Tolerance to consider eigenvectors equal to zero (Defaults to 1e-7, see the function \link{control()})
#' @param \dots Additional optional arguments (See the function \link{glm})
#' @export

fglm <- function(formula, data, family = gaussian(), weights = NULL,
                           start = NULL, etastart = NULL, mustart = NULL, offset = NULL, maxit = 25, k = 2,
                           model = TRUE, method = c("eigen", "Cholesky", "qr"),
                           x = FALSE, y = TRUE,
                           tol.estimation = 1e-8, tol.solve = .Machine$double.eps,
                           tol.values = 1e-7, tol.vectors = 1e-7, ...) {
  call <- match.call()
  target <- y
  M <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset"), names(M), 0L)
  M <- M[c(1L, m)]
  M$drop.unused.levels <- TRUE
  M[[1L]] <- quote(stats::model.frame)
  M <- eval(M, parent.frame())
  y <- M[[1]]
  tf <- attr(M, "terms")
  X <- model.matrix(tf, M)
  offset <- model.offset(M)
  intercept <- attributes(tf)$intercept
  method <- match.arg(method)
  rval <- fglm.wfit(
    X = X,
    y = y,
    family = family,
    weights = weights,
    start = start,
    etastart = etastart,
    mustart = mustart,
    offset = offset,
    intercept = intercept,
    maxit = maxit,
    k = k,
    tol.estimation = tol.estimation,
    tol.solve = tol.solve,
    tol.values = tol.values,
    tol.vectors = tol.vectors,
    method = method
  )
  rval$terms <- tf
  rval$call <- call
  class(rval) <- c("fglm", "flm")
  if (model) rval$model <- M
  if (x) rval$x <- X
  if (target) rval$y <- y

  rval$fitted.values <- predict.fglm(rval, newdata = M, type = "response")
  rval$linear.predictors <- predict.fglm(rval, newdata = M, type = "link")

  if ((rval$iter == maxit) & (!rval$convergence)) {
    warning("Maximum number of iterations reached without convergence")
  }
  rval
}
