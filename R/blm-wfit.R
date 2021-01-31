blm.wfit <- function(y, X, w, intercept = FALSE, offset = NULL,
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
        stop("blm.fit: Unknown singularity.method value")
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
  class(rval) <- "blm"
  rval
}
