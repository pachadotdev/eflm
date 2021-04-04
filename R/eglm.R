#' Effiicient Fitting of Generalized Linear Models
#'
#' Efficient generalized linear model (\code{"eglm"}) is used to fit generalized
#' linear models in an equivalent way to \code{"\link{glm}"} but in a reduced
#' time depending on the design matrix (see the DESCRIPTION).
#'
#' @rdname eflm
#' @return an object of class "elm" that behaves the same way as the "lm" class,
#' see the function \link{glm}.
#' @examples
#' # Generalized linear model with Gaussian link
#' eglm(mpg ~ wt, family = gaussian(), data = mtcars)
#' @importFrom stats gaussian na.pass
#' @export
eglm <- function(formula, data, family = gaussian(), weights = NULL,
                 subset = NULL,
                 intercept = TRUE,
                 na.action = na.omit, start = NULL, etastart = NULL,
                 mustart = NULL, offset = NULL, maxit = 25, k = 2, model = TRUE,
                 singularity.method = c("eigen", "Cholesky", "qr"),
                 x = FALSE, y = TRUE,
                 tol.estimation = 1e-8, tol.solve = .Machine$double.eps,
                 tol.values = 1e-7, tol.vectors = 1e-7, ...) {
  call <- match.call()
  target <- y
  M <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "weights", "subset", "na.action", "etastart",
               "mustart", "offset"), names(M), 0L)
  M <- M[c(1L, m)]
  M$drop.unused.levels <- TRUE
  M[[1L]] <- quote(stats::model.frame)
  M <- eval(M, parent.frame())
  y <- M[[1]]
  tf <- attr(M, "terms")
  X <- model.matrix(tf, M)
  offset <- model.offset(M)
  intercept <- attributes(tf)$intercept
  singularity.method <- match.arg(singularity.method)
  rval <- eglm.wfit(
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
    singularity.method = singularity.method
  )
  rval$terms <- tf
  rval$call <- call
  if (model) rval$model <- M
  rval$fitted.values <- predict(rval, newdata = data, type = "response", na.action = na.action)
  rval$linear.predictors <- predict(rval, newdata = data, type = "link", na.action = na.action)
  if (x) rval$x <- X
  if (target) {
    rval$y <- y
    names(rval$y) <- names(rval$fitted.values)
  }
  names(rval$prior.weights) <- names(rval$fitted.values)
  qr_tol <- 1e-11
  rval$qr <- qr(model.matrix(rval), tol = qr_tol)
  attr(rval$qr$qr, "assign") <- NULL
  rval$qr$tol <- qr_tol
  attr(rval$qr, "qr") <- "qr"

  if ((rval$iter == maxit) & (!rval$convergence)) {
    warning("Maximum number of iterations reached without convergence")
  }
  rval
}
