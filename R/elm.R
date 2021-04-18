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
#' @details Models for \code{elm} and \code{eglm} are specified symbolically.
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
#'  \code{"\link{lm}"} and \code{"\link{glm}"} from the \link{stats} package.
#' @return an object of class "elm" that behaves the same way as the "lm" class,
#'  see the function \link{lm}.
#'
#' @examples
#' elm(mpg ~ wt, data = mtcars)
#' @importFrom stats gaussian na.pass
#'
#' @export
elm <- function(formula, data, subset, weights, na.action,
                method = "qr", model = TRUE, x = FALSE, y = FALSE,
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
  if (method == "model.frame") {
    return(mf)
  } else if (method != "qr") {
    warning(gettextf("method = '%s' is not supported. Using 'qr'", method),
      domain = NA
    )
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
  }
  else {
    x <- model.matrix(mt, mf, contrasts)
    ## unlike stats::, here w is always passed to elm.wfit
    z <- elm.wfit(x, y, w,
      offset = offset, singular.ok = singular.ok,
      reduce = reduce, ...
    )
  }
  class(z) <- c("elm", if (mlm) "melm", "lm")
  z$na.action <- attr(mf, "na.action")
  z$offset <- offset
  z$contrasts <- attr(x, "contrasts")
  z$xlevels <- .getXlevels(mt, mf)
  z$call <- cl
  z$terms <- mt
  if (model) {
    z$model <- mf
  }
  z$reduce <- reduce
  if (ret.x) {
    z$x <- x
  }
  if (ret.y) {
    z$y <- y
  }
  if (!qr) z$qr <- NULL
  if (isTRUE(reduce)) {
    z$xtx <- crossprod(x)
    z$qr$original.dimensions <- dim(x)
  }
  z
}
