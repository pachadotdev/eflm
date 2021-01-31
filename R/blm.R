#' Fitting Linear Models
#'
#' blm is used to fit linear models.
#'
#' @param formula A formula for the model
#' @param data A tibble or data.frame
#' @param intercept Logical value to determine wheareas to included an intercept in the null model (Defaults to \code{TRUE})
#' @param weights An optional vector of ‘prior weights’ to be used in the fitting process. Should be \code{NULL}
#' or a numeric vector (e.g. \code{data$weights}, defaults to \code{NULL})
#' @param na.action a function which indicates what should happen when the data
#' contain NAs. The default is set by the \code{na.action} setting of options,
#' and is \link{na.fail} if that is unset. The ‘factory-fresh’ default is
#' \code{na.omit}. Another possible value is NULL, no action. Value
#' \code{na.exclude} can be useful.
#' @param offset This can be used to specify an a priori known component to be included in the
#' linear predictor during fitting. This should be \code{NULL} or a numeric vector of length
#' equal to the number of cases. One or more offset terms can be included in the formula instead
#' or as well, and if more than one is specified their sum is used. See \code{stats::model.offset()}
#' @param model A logical value indicating whether model frame should be included as a
#' component of the returned value (Defaults to \code{TRUE})
#' @param singularity.method The chosen method to detect for singularity. Defaults to \code{"eigen"} but
#' it can also be \code{"Cholesky"} or \code{"qr"}
#' @param x Logical value indicating whether the model matrix used in the fitting process
#' should be returned as components of the returned value (Defaults to \code{FALSE}, see the function \link{glm.fit})
#' @param y Logical value indicating whether the response vector used in the fitting process
#' should be returned as components of the returned value (Defaults to \code{FALSE}, see the function \link{glm.fit})
#' @param tol.solve (Defaults to \code{.Machine$double.eps}, see the function \link{solve})
#' @param tol.values Tolerance to consider eigenvalues equal to zero (Defaults to 1e-7, see the function \link{control}),
#' @param tol.vectors Tolerance to consider eigenvectors equal to zero (Defaults to 1e-7, see the function \link{control})
#' @param \dots For blm: arguments to be used to form the default control argument if it is not supplied directly. For weights: further arguments passed to or from other methods.
#' @importFrom stats gaussian na.pass
#' @export
blm <- function(formula, data, intercept = TRUE, weights = NULL,
                na.action = na.omit, offset = NULL,
                model = TRUE, singularity.method = c("eigen", "Cholesky", "qr"),
                x = FALSE, y = TRUE,
                tol.solve = .Machine$double.eps,
                tol.values = 1e-7, tol.vectors = 1e-7, ...) {
  target <- y
  call <- match.call()
  M <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data"), names(M), 0L)
  M <- M[c(1L, m)]
  M$drop.unused.levels <- TRUE
  M[[1L]] <- quote(stats::model.frame)
  M <- eval(M, parent.frame())
  y <- M[[1]]
  tf <- attr(M, "terms")
  X <- model.matrix(tf, M)
  offset <- model.offset(M)
  if (is.null(offset)) {
    offset <- rep(0, length(y))
  }
  if (is.null(weights)) weights <- rep(1, length(y))
  rval <- blm.wfit(
    y,
    X,
    offset = offset,
    w = weights,
    tol.solve = tol.solve,
    tol.values = tol.values,
    tol.vectors = tol.vectors,
    singularity.method = singularity.method,
    intercept = attr(tf, "intercept")
  )

  rval$terms <- tf
  rval$call <- call
  if (ncol(M) > 1) {
    for (i in 2:ncol(M)) {
      if (is.factor(M[, i])) {
        eval(parse(text = paste("rval$levels$'", names(M)[i],
          "'", "<-levels(M[,i])",
          sep = ""
        )))
      }
    }
  }
  if (model) rval$model <- M
  if (x) rval$x <- X
  if (target) rval$y <- y
  class(rval) <- "blm"
  rval$fitted.values <- predict.blm(rval, M)
  rval$residuals <- y - rval$fitted.values
  rval$formula <- eval(call[[2]])
  rval
}
