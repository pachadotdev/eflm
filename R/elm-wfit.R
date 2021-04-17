#' @rdname model_fitting
#' @export
elm.wfit <- function(x, y, w, offset = NULL, method = "qr", tol = 1e-7,
                     singular.ok = TRUE, ...) {
  if (is.null(n <- nrow(x))) stop("'x' must be a matrix")
  if (n == 0) stop("0 (non-NA) cases")
  ny <- NCOL(y)
  ## treat one-col matrix as vector
  if (is.matrix(y) && ny == 1L) {
    y <- drop(y)
  }
  if (!is.null(offset)) {
    y <- y - offset
  }
  if (NROW(y) != n | length(w) != n) {
    stop("incompatible dimensions")
  }
  if (any(w < 0 | is.na(w))) {
    stop("missing or negative weights not allowed")
  }
  if (method != "qr") {
    warning(gettextf("method = '%s' is not supported. Using 'qr'", method),
      domain = NA
    )
  }
  chkDots(...)
  x.asgn <- attr(x, "assign") # save
  zero.weights <- any(w == 0)
  if (zero.weights) {
    save.r <- y
    save.f <- y
    save.w <- w
    ok <- w != 0
    nok <- !ok
    w <- w[ok]
    x0 <- x[!ok, , drop = FALSE]
    x <- x[ok, , drop = FALSE]
    n <- nrow(x)
    y0 <- if (ny > 1L) y[!ok, , drop = FALSE] else y[!ok]
    y <- if (ny > 1L) y[ok, , drop = FALSE] else y[ok]
  }
  p <- ncol(x)
  if (p == 0) {
    ## oops, null model
    return(list(
      coefficients = numeric(), residuals = y,
      fitted.values = 0 * y, weights = w, rank = 0L,
      df.residual = length(y)
    ))
  }
  if (n == 0) { # all cases have weight zero
    return(list(
      coefficients = rep(NA_real_, p), residuals = y,
      fitted.values = 0 * y, weights = w, rank = 0L,
      df.residual = 0L
    ))
  }
  wts <- sqrt(w)
  C_Cdqrls <- getNativeSymbolInfo("Cdqrls", PACKAGE = getLoadedDLLs()$stats)
  z <- .Call(C_Cdqrls, x * wts, y * wts, tol, FALSE)
  if (!singular.ok && z$rank < p) stop("singular fit encountered")
  coef <- z$coefficients
  pivot <- z$pivot
  r1 <- seq_len(z$rank)
  dn <- colnames(x)
  if (is.null(dn)) dn <- paste0("x", 1L:p)
  nmeffects <- c(dn[pivot[r1]], rep.int("", n - z$rank))
  r2 <- if (z$rank < p) (z$rank + 1L):p else integer()
  if (is.matrix(y)) {
    coef[r2, ] <- NA
    if (z$pivoted) coef[pivot, ] <- coef
    dimnames(coef) <- list(dn, colnames(y))
    dimnames(z$effects) <- list(nmeffects, colnames(y))
  } else {
    coef[r2] <- NA
    if (z$pivoted) coef[pivot] <- coef
    names(coef) <- dn
    names(z$effects) <- nmeffects
  }
  z$coefficients <- coef
  z$residuals <- z$residuals / wts
  z$fitted.values <- y - z$residuals
  z$weights <- w
  if (zero.weights) {
    coef[is.na(coef)] <- 0
    f0 <- x0 %*% coef
    if (ny > 1) {
      save.r[ok, ] <- z$residuals
      save.r[nok, ] <- y0 - f0
      save.f[ok, ] <- z$fitted.values
      save.f[nok, ] <- f0
    }
    else {
      save.r[ok] <- z$residuals
      save.r[nok] <- y0 - f0
      save.f[ok] <- z$fitted.values
      save.f[nok] <- f0
    }
    z$residuals <- save.r
    z$fitted.values <- save.f
    z$weights <- save.w
  }
  if (!is.null(offset)) {
    z$fitted.values <- z$fitted.values + offset
  }
  if (z$pivoted) colnames(z$qr) <- colnames(x)[z$pivot]
  qr <- z[c("qr", "qraux", "pivot", "tol", "rank")]
  c(
    z[c(
      "coefficients", "residuals", "fitted.values", "effects",
      "weights", "rank"
    )],
    list(
      assign = x.asgn,
      qr = structure(qr, class = "qr"),
      df.residual = n - z$rank
    )
  )
}
