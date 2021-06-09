#' Fitting Linear Models
#'
#' Efficient Linear Model Weighted Fit (\code{"elm.wfit"}) is used
#'  to fit linear models in an equivalent way to \code{"\link{glm.fit}"} but in
#'  a reduced time depending on the design matrix and the family (or link).
#'
#' @inheritParams elm
#' @inheritParams eglm.wfit
#' @param tol tolerance for the \code{\link{qr}} decomposition. Default is 1e-7.
#'
#' @details \code{elm.wfit} is a workhorse function: it is not normally called
#'  directly but can be more efficient where the response vector, design matrix
#'  and family have already been calculated. Use \code{elm} for most of the
#'  cases.
#'
#' @return A list that contains the same elements as the output from
#'  \code{"\link{lm.fit}"}.
#'
#' @examples
#' x <- cbind(rep(1, nrow(mtcars)), mtcars$wt)
#' y <- mtcars$mpg
#' elm.wfit(x, y)
#' @export
elm.wfit <- function(x, y, weights = rep.int(1, n), offset = NULL, method = c("qr","chol"), tol = 1e-7,
                     singular.ok = TRUE, reduce = TRUE, ...) {
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
  if (NROW(y) != n | length(weights) != n) {
    stop("incompatible dimensions")
  }
  if (any(weights < 0 | is.na(weights))) {
    stop("missing or negative weights not allowed")
  }
  if (method != "qr") {
    warning(gettextf("method = '%s' is not supported. Using 'qr'", method),
            domain = NA
    )
  }
  chkDots(...)
  x.asgn <- attr(x, "assign") # save
  zero.weights <- any(weights == 0)
  if (zero.weights) {
    save.r <- y
    save.f <- y
    save.weights <- weights
    ok <- weights != 0
    nok <- !ok
    weights <- weights[ok]
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
      fitted.values = 0 * y, weights = weights, rank = 0L,
      df.residual = length(y)
    ))
  }
  if (n == 0) { # all cases have weight zero
    return(list(
      coefficients = rep(NA_real_, p), residuals = y,
      fitted.values = 0 * y, weights = weights, rank = 0L,
      df.residual = 0L
    ))
  }

  if (method == "qr") {
    C_Cdqrls <- getNativeSymbolInfo("Cdqrls", PACKAGE = getLoadedDLLs()$stats)
    z <- if (isTRUE(reduce)) {
      # here I pass weights on the right side to avoid duplicating operations, because here (sqrt(w) * x , sqrt(w) * y) = (x , w * y)
      .Call(C_Cdqrls, crossprod(x * sqrt(weights)), crossprod(x, y * weights), tol, FALSE)
    } else {
      .Call(C_Cdqrls, x * sqrt(weights), y * sqrt(weights), tol, FALSE)
    }

    if (!singular.ok && z$rank < p) stop("singular fit encountered")
    coef <- z$coefficients
    pivot <- z$pivot
    r1 <- seq_len(z$rank)
    dn <- colnames(x)
    if (is.null(dn)) dn <- paste0("x", 1L:p)
    # for nmeffects I used ncol(x) when reduce = T, because nrow((wXw)t(wXw)) = ncol(X)
    nmeffects <- c(dn[pivot[r1]], rep.int(
      "",
      if (isTRUE(reduce)) ncol(x) - z$rank else n - z$rank
    ))
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
    if (isTRUE(reduce)) {
      z$fitted.values <- as.numeric(x %*% matrix(z$coefficients, ncol = 1))
      # the division by weights is not included for the residuals, as this
      # part of the ifelse statement starts from fitted values
      z$residuals <- as.numeric(y - z$fitted.values)
      names(z$fitted.values) <- rownames(x)
      names(z$residuals) <- rownames(x)
    } else {
      z$residuals <- z$residuals / sqrt(weights)
      z$fitted.values <- y - z$residuals
    }
    z$weights <- weights
    if (zero.weights) {
      coef[is.na(coef)] <- 0
      f0 <- x0 %*% coef
      if (ny > 1) {
        save.r[ok, ] <- z$residuals
        save.r[nok, ] <- y0 - f0
        save.f[ok, ] <- z$fitted.values
        save.f[nok, ] <- f0
      } else {
        save.r[ok] <- z$residuals
        save.r[nok] <- y0 - f0
        save.f[ok] <- z$fitted.values
        save.f[nok] <- f0
      }
      z$residuals <- save.r
      z$fitted.values <- save.f
      z$weights <- save.weights
    }
    if (!is.null(offset)) {
      z$fitted.values <- z$fitted.values + offset
    }

    if (z$pivoted) colnames(z$qr) <- colnames(x)[z$pivot]
    qr <- z[c("qr", "qraux", "pivot", "tol", "rank")]
    return(
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
    )
  }

  if (method == "chol") {
    z <- list()
    z$xtx <- chol(crossprod(x * sqrt(weights)), pivot = TRUE)
    z$pivot <- attributes(z$xtx)$"pivot"
    z$pivoted <- TRUE
    z$rank <- attributes(z$xtx)$"rank"
    z$coefficients <- as(solve(z$xtx, crossprod(x, y * weights)), "numeric")
  }
}
