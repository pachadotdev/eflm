#' @importFrom stats getCall
update_with_more_data <- function(object, data, weights = NULL, offset = NULL,
                               all.levels = FALSE,
                               singularity.method = c("eigen", "Cholesky", "qr"),
                               tol.solve = .Machine$double.eps,
                               tol.values = 1e-7, tol.vectors = 1e-7, ...) {
  if (!inherits(object, "elm")) {
    stop("object must be of class elm")
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
  class(rval) <- c("elm", "update_with_more_data")
  rval
}
