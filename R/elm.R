#' Efficient Fitting of Linear Models
#'
#' Efficient Linear Model (\code{"elm"}) is used to fit linear
#'  models in an equivalent way to \code{"\link{lm}"} but in a reduced time
#'  depending on the design matrix.
#'
#' @inheritParams eglm
#' @param qr logical. If TRUE the corresponding QR decomposition component of
#'  the fit is returned.
#'
#' @details Models for \code{elm} are specified symbolically.
#'  A typical model has the form \code{response ~ terms} where \code{response}
#'  is the (numeric) response vector and \code{terms} is a series of terms which
#'  specifies a linear predictor for \code{response}. A terms specification of
#'  the form \code{first + second} indicates all the terms in \code{first}
#'  together with all the terms in \code{second} with duplicates removed. A
#'  specification of the form \code{first:second} indicates the set of
#'  terms obtained by taking the interactions of all terms in \code{first}
#'  with all terms in \code{second}. The specification \code{first*second}
#'  indicates the \emph{cross} of \code{first} and \code{second}. This is
#'  the same as \code{first + second + first:second}, and exactly the same as
#'  \code{"\link{lm}"} from the \link{stats} package.
#'
#' @return An object of class "elm" that behaves the same way as the "lm"
#'  class, see the function \code{"\link{lm}"}. This output also includes the
#'  logical "reduce" and, depending on it, the reduced design matrix "xtx"
#'  when the reduce argument is set to TRUE.
#'
#' @examples
#' elm(mpg ~ wt, data = mtcars)
#' @export
elm <- function(formula, data, subset, weights, na.action,
                method = c("qr","chol"), model = TRUE, x = FALSE, y = FALSE,
                qr = TRUE, singular.ok = TRUE, contrasts = NULL,
                offset, reduce = TRUE, ...) {
  ret.x <- x
  ret.y <- y
  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(
    c("formula", "data", "subset", "weights", "na.action", "offset"),
    names(mf), 0L
  )
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  ## need stats:: for non-standard evaluation
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  if (any(method %in% "model.frame")) {
    return(mf)
  } else {
    method <- match.arg(method)
  }
  mt <- attr(mf, "terms") # allow model.frame to update it
  y <- model.response(mf, "numeric")
  ## avoid any problems with 1D or nx1 arrays by as.vector.
  w <- as.vector(model.weights(mf))
  ## w will be passed straight to elm.wfit, there is no elm.fit in this package
  if (is.null(w)) w <- rep.int(1, length(y))
  if (!is.null(w) && !is.numeric(w)) {
    stop("'weights' must be a numeric vector")
  }
  offset <- model.offset(mf)
  mlm <- is.matrix(y)
  ny <- if (mlm) nrow(y) else length(y)
  if (!is.null(offset)) {
    if (!mlm) offset <- as.vector(offset)
    if (NROW(offset) != ny) {
      stop(gettextf(
        "number of offsets is %d, should equal %d (number of observations)",
        NROW(offset), ny
      ), domain = NA)
    }
  }

  if (is.empty.model(mt)) {
    x <- NULL
    z <- list(
      coefficients = if (mlm) {
        matrix(NA_real_, 0, ncol(y))
      } else {
        numeric()
      },
      residuals = y,
      fitted.values = 0 * y, weights = w, rank = 0L,
      df.residual = if (!is.null(w)) sum(w != 0) else ny
    )
    if (!is.null(offset)) {
      z$fitted.values <- offset
      z$residuals <- y - offset
    }
  } else {
    x <- model.matrix(mt, mf, contrasts)
    ## unlike stats::, here w is always passed to elm.wfit
    z <- elm.wfit(
      x = x, y = y, weights = w,
      offset = offset, singular.ok = singular.ok,
      reduce = reduce, method = method, ...
    )
  }
  class(z) <- c("elm", "lm", if (mlm) "melm")
  z$na.action <- attr(mf, "na.action")
  z$offset <- offset
  z$contrasts <- attr(x, "contrasts")
  z$xlevels <- .getXlevels(mt, mf)
  z$call <- cl
  z$terms <- mt
  if (model) {
    z$model <- mf
  }
  if (ret.x) {
    z$x <- x
  }
  if (ret.y) {
    z$y <- y
  }
  if (!qr) z$qr <- NULL
  z$reduce <- reduce
  if (isTRUE(reduce)) {
    z$xtx <- crossprod(sqrt(z$weights) * x)
    z$qr$original.dimensions <- dim(x)
  }
  return(z)
}
