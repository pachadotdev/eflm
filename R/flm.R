#' Fitting Linear Models
#'
#' flm is used to fit linear models.
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
#' @param singularity.method The chosen method to detect for singularity. Defaults to \code{"eigen"} but
#' it can also be \code{"Cholesky"} or \code{"qr"}
#' @param x Logical value indicating whether the model matrix used in the fitting process
#' should be returned as components of the returned value (Defaults to \code{FALSE}, see the function \link{glm.fit})
#' @param y Logical value indicating whether the response vector used in the fitting process
#' should be returned as components of the returned value (Defaults to \code{FALSE}, see the function \link{glm.fit})
#' @param tol.solve (Defaults to \code{.Machine$double.eps}, see the function \link{solve})
#' @param tol.values Tolerance to consider eigenvalues equal to zero (Defaults to 1e-7, see the function \link{control}),
#' @param tol.vectors Tolerance to consider eigenvectors equal to zero (Defaults to 1e-7, see the function \link{control})
#' @param \dots For glm: arguments to be used to form the default control argument if it is not supplied directly. For weights: further arguments passed to or from other methods.
#' @importFrom stats gaussian na.pass
#' @export
flm <- function(formula, data, intercept = TRUE, weights = NULL,
                na.action = na.omit, offset = NULL,
                model = TRUE, method = c("eigen", "Cholesky", "qr"),
                x = FALSE, y = TRUE, fitted = FALSE,
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
  rval <- flm.wfit(y, X,
    offset = offset, w = weights,
    tol.solve = tol.solve,
    tol.values = tol.values, tol.vectors = tol.vectors,
    method = method, intercept = attr(tf, "intercept")
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
  class(rval) <- "flm"
  rval$fitted.values <- predict.flm(rval, M)
  rval$residuals <- y - rval$fitted.values
  rval$formula <- eval(call[[2]])
  rval
}

flm.wfit <- function(y, X, w, intercept = FALSE, offset = NULL,
                     tol.solve = .Machine$double.eps, tol.values = 1e-07,
                     tol.vectors = 1e-07, singularity.method = c("eigen", "Cholesky", "qr"), ...) {
  nvar <- ncol(X)
  nobs <- nrow(X)
  if (is.null(offset)) {
    offset <- rep(0, length(y))
  }
  if (any(is.na(w))) {
    stop("some weights are NA")
  }
  if (any(w < 0)) {
    stop("weights must not be negative")
  }
  colnam <- colnames(X)
  pw <- sum(log(w[w != 0]))
  sqw <- sqrt(w)
  sqwX <- sqw * X
  SW <- sum(w)
  yy <- crossprod(sqw * y)
  X1X <- colSums(sqwX)
  names(X1X) <- colnam
  XW1 <- crossprod(w, X)
  A <- cp(sqwX)
  y <- y - offset
  Xy <- t(crossprod((w * y), X))
  singularity.method <- match.arg(singularity.method)
  if (singularity.method == "eigen") {
    ris <- control(A, , tol.values, tol.vectors, , singularity.method)
    ris$XTX <- as(ris$XTX, "matrix")
    ok <- ris$pivot[1:ris$rank]
    coef <- as(solve(ris$XTX, Xy[ok]), "numeric")
    coefficients <- rep(NA, nvar)
    coefficients[ok] <- coef
    RSS <- yy - 2 * crossprod(coef, Xy[ok]) + t(coef) %*% ris$XTX %*% coef
  } else
  if (singularity.method == "Cholesky") {
    ris <- control(A, , tol.values, tol.vectors, , singularity.method)
    ris$XTX <- as(ris$XTX, "matrix")
    ok <- ris$pivot[1:ris$rank]
    coef <- as(solve(ris$XTX, Xy[ok]), "numeric")
    coefficients <- rep(NA, nvar)
    coefficients[ok] <- coef
    RSS <- yy - 2 * crossprod(coef, Xy[ok]) + t(coef) %*% ris$XTX %*% coef
  } else
  if (singularity.method == "qr") {
    if (class(A) == "dsCMatrix") {
      A <- as(A, "matrix")
      Xy <- as(Xy, "matrix")
    }
    C_Cdqrls <- getNativeSymbolInfo("Cdqrls", PACKAGE = getLoadedDLLs()$stats)
    ris <- c(list(XTX = A), .Call(C_Cdqrls, A, Xy, tol.values, FALSE))
    coefficients <- ris$coefficients
    coef <- coefficients[ris$pivot[1:ris$rank]]
    ord <- order(ris$pivot)
    RSS <- yy - 2 * crossprod(coefficients, Xy[ris$pivot]) + t(coefficients[ord]) %*% ris$XTX %*% coefficients[ord]
    ok <- ris$pivot[1:ris$rank]
    if (ris$rank < nvar) {
      coefficients[(ris$rank + 1L):nvar] <- NA
    }
    coefficients <- coefficients[ord]
  } else {
    stop("flm.fit: Unknown singularity.method value")
  }

  names(coefficients) <- colnames(X)
  zero.w <- sum(w == 0)
  dfr <- nrow(X) - ris$rank - zero.w
  rval <- list(
    coefficients = coefficients,
    weights = w,
    df.residual = dfr,
    XTX = as(ris$XTX, "matrix"),
    Xy = Xy,
    nobs = nrow(X),
    nvar = nvar,
    ok = ok,
    A = as(A, "matrix"),
    RSS = as.numeric(RSS),
    rank = ris$rank,
    pivot = ris$pivot,
    yy = yy,
    X1X = X1X,
    SW = SW,
    XW1 = XW1,
    zero.w = zero.w,
    pw = pw,
    intercept = intercept,
    singularity.method = singularity.method
  )
  class(rval) <- "flm"
  rval
}

formula.flm <- function(x, ...) {
  form <- formula(x$terms)
  environment(form) <- environment(x$formula)
  form
}

update.flm <- function(object, formula, data, add = TRUE, evaluate = TRUE, ...) {
  if (!inherits(object, "flm")) {
    stop("object must be of class flm")
  }
  if ((!missing(formula)) & (!missing(data)) & (add)) {
    stop("cannot specify a formula while adding new data")
  }
  if ((!missing(data)) & (add)) {
    mod <- updateWithMoreData(object, data, formula = formula.flm(object), ...)
  }
  else {
    mod <- if (missing(data)) {
      update.default(object, formula, evaluate = evaluate, ...)
    } else {
      update.default(object, formula, data = data, evaluate = evaluate, ...)
    }
  }
  mod
}

updateWithMoreData <- function(object, data, weights = NULL, offset = NULL,
                               all.levels = FALSE, ...) {
  if (!inherits(object, "flm")) {
    stop("object must be of class flm")
  }
  if (is.null(call <- getCall(object))) {
    stop("need an object with call component")
  }
  M <- match.call(expand.dots = F)
  formula <- eval(object$call[[2]])
  M$formula <- formula
  m <- match(c("formula", "data"), names(M), 0L)
  M <- M[c(1L, m)]
  M$drop.unused.levels <- TRUE
  M[[1L]] <- quote(stats::model.frame)
  M <- eval(M, parent.frame())
  y <- M[[1]]
  fa <- which(attributes(attributes(M)$terms)$dataClasses == "factor")
  if ((!(all.levels)) & (length(fa) > 0)) {
    flevels <- list()
    j <- 0
    for (i in 1:length(fa)) {
      j <- j + 1
      eval(parse(text = paste("flevels$'", names(M)[fa[i]],
        "'", "<-levels(M[,fa[i]])",
        sep = ""
      )))
      a <- c(object$levels[[j]][!(object$levels[[j]] %in% flevels[[j]])], flevels[[j]])
      flevels[[j]] <- a
    }
    M <- model.frame(formula, data, xlev = flevels)
    X <- model.matrix(formula, M, xlev = flevels)
    object$levels <- flevels
  } else {
    X <- model.matrix(object$terms, M)
    flevels <- object$levels
  }
  pw <- if (is.null(weights)) {
    weights
  } else {
    sum(log(weights[weights != 0])) + object$pw
  }
  w <- weights
  zero.w <- sum(w == 0)
  offset <- model.offset(M)
  colnam <- colnames(X)
  if (!is.null(w)) {
    if (object$rank == ncol(X)) {
      XTX <- cp(X, w)
      XTX <- as(XTX, "matrix") + object$XTX
      A <- XTX
      Xy <- as(object$Xy, "matrix") + as(crossprod(X, (w * y)), "matrix")
      rank <- object$rank
      ok <- 1:ncol(X)
      nvar <- object$nvar
    } else {
      A <- cp(X, w)
      A <- as(A, "matrix")
      A[rownames(object$A), colnames(object$A)] <- A[
        rownames(object$A),
        colnames(object$A)
      ] + object$A
      ris <- control(A, , tol.values, tol.vectors, , singularity.method)
      XTX <- ris$XTX
      ok <- ris$pivot[1:ris$rank]
      Xy <- as(crossprod(X, (w * y)), "matrix")
      Xy[rownames(object$A), ] <- Xy[rownames(object$A), ] + as(object$Xy, "matrix")
      rank <- ris$rank
      nvar <- length(ris$pivot)
    }
    sqw <- sqrt(w)
    coef <- solve(XTX, Xy[ok], tol = tol.solve)
    yy <- object$yy + crossprod(sqw * y)
    X1X <- crossprod(sqw, X)
    names(X1X) <- colnam
    X1X[names(object$X1X)] <- X1X[names(object$X1X)] + object$X1X
    XW1 <- crossprod(w, X)
    names(XW1) <- colnam
    XW1[names(object$X1X)] <- XW1[names(object$X1X)] + object$XW1
    SW <- sum(w) + object$SW
    dfr <- object$df.residual + nrow(X) - zero.w
  } else {
    if (object$rank == ncol(X)) {
      XTX <- cp(X, w = NULL)
      XTX <- as(XTX, "matrix") + object$XTX
      A <- XTX
      Xy <- as(object$Xy, "matrix") + as(crossprod(X, y), "matrix")
      rank <- object$rank
      ok <- 1:ncol(X)
      nvar <- object$nvar
    } else {
      A <- cp(X, w = NULL)
      A <- as(A, "matrix")

      A[rownames(object$A), colnames(object$A)] <- A[
        rownames(object$A),
        colnames(object$A)
      ] + object$A
      ris <- control(A, , tol.values, tol.vectors, , singularity.method)
      XTX <- ris$XTX
      ok <- ris$pivot[1:ris$rank]
      Xy <- as(crossprod(X, y), "matrix")
      Xy[rownames(object$A), ] <- Xy[rownames(object$A), ] + as(object$Xy, "matrix")
      rank <- ris$rank
      nvar <- length(ris$pivot)
    }
    coef <- solve(XTX, Xy[ok], tol = tol.solve)
    yy <- object$yy + crossprod(y)
    X1X <- colSums(X)
    names(X1X) <- colnam
    X1X[names(object$X1X)] <- X1X[names(object$X1X)] + object$X1X
    dfr <- object$nobs + nrow(X) - rank
    XW1 <- SW <- NULL
  }
  RSS <- yy - 2 * crossprod(coef, Xy[ok]) + crossprod(
    coef,
    XTX
  ) %*% coef
  coefficients <- rep(NA, nvar)
  coefficients[ok] <- coef
  names(coefficients) <- colnames(X)
  rval <- list(
    coefficients = coefficients,
    df.residual = dfr,
    X1X = X1X, Xy = Xy, XW1 = XW1, SW = SW, yy = yy,
    A = as(A, "matrix"), nobs = object$nobs + nrow(X), RSS = as.numeric(RSS),
    rank = rank, ok = ok, nvar = nvar, weights = weights,
    zero.w = zero.w + object$zero.w, pw = pw, XTX = as(XTX, "matrix"),
    "intercept" = object$intercept, singularity.method = singularity.method
  )
  rval$terms <- object$terms
  rval$call <- call
  rval$levels <- flevels
  class(rval) <- c("flm", "updateWithMoreData")
  rval
}
