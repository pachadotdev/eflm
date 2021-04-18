#' @export
#' @noRd
summary.eglm <- function(object, dispersion = NULL,
                         correlation = FALSE, symbolic.cor = FALSE, ...) {
  est.disp <- FALSE
  df.r <- object$df.residual
  if (is.null(dispersion)) { # calculate dispersion if needed
    dispersion <-
      if (object$family$family %in% c("poisson", "binomial")) {
        1
      } else if (df.r > 0) {
        est.disp <- TRUE
        if (any(object$weights == 0)) {
          warning("observations with zero weight not used for calculating dispersion")
        }
        sum((object$weights * object$residuals^2)[object$weights > 0]) / df.r
      } else {
        est.disp <- TRUE
        NaN
      }
  }

  ## calculate scaled and unscaled covariance matrix

  aliased <- is.na(coef(object)) # used in print method
  p <- object$rank
  if (p > 0) {
    p1 <- 1L:p
    Qr <- qr.elm(object)
    ## WATCHIT! doesn't this rely on pivoting not permuting 1L:p? -- that's quaranteed
    coef.p <- object$coefficients[Qr$pivot[p1]]
    covmat.unscaled <- if (isTRUE(object$reduce)) {
        unname(solve.qr(qr(object$xtx, LAPACK = T)))
      } else {
        chol2inv(Qr$qr[p1, p1, drop = FALSE])
      }
    dimnames(covmat.unscaled) <- list(names(coef.p), names(coef.p))
    covmat <- dispersion * covmat.unscaled
    var.cf <- diag(covmat)

    ## calculate coef table

    s.err <- sqrt(var.cf)
    tvalue <- coef.p / s.err

    dn <- c("Estimate", "Std. Error")
    if (!est.disp) { # known dispersion
      pvalue <- 2 * pnorm(-abs(tvalue))
      coef.table <- cbind(coef.p, s.err, tvalue, pvalue)
      dimnames(coef.table) <- list(
        names(coef.p),
        c(dn, "z value", "Pr(>|z|)")
      )
    } else if (df.r > 0) {
      pvalue <- 2 * pt(-abs(tvalue), df.r)
      coef.table <- cbind(coef.p, s.err, tvalue, pvalue)
      dimnames(coef.table) <- list(
        names(coef.p),
        c(dn, "t value", "Pr(>|t|)")
      )
    } else { # df.r == 0
      coef.table <- cbind(coef.p, NaN, NaN, NaN)
      dimnames(coef.table) <- list(
        names(coef.p),
        c(dn, "t value", "Pr(>|t|)")
      )
    }
    df.f <- NCOL(Qr$qr)
  } else {
    coef.table <- matrix(, 0L, 4L)
    dimnames(coef.table) <- list(NULL,
      c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
    covmat.unscaled <- covmat <- matrix(, 0L, 0L)
    df.f <- length(aliased)
  }
  ## return answer

  ## these need not all exist, e.g. na.action.
  keep <- match(c(
    "call", "terms", "family", "deviance", "aic",
    "contrasts", "df.residual", "null.deviance", "df.null",
    "iter", "na.action"
  ), names(object), 0L)
  ans <- c(
    object[keep],
    list(
      deviance.resid = residuals(object, type = "deviance"),
      coefficients = coef.table,
      aliased = aliased,
      dispersion = dispersion,
      df = c(object$rank, df.r, df.f),
      cov.unscaled = covmat.unscaled,
      cov.scaled = covmat
    )
  )

  if (correlation && p > 0) {
    dd <- sqrt(diag(covmat.unscaled))
    ans$correlation <-
      covmat.unscaled / outer(dd, dd)
    ans$symbolic.cor <- symbolic.cor
  }
  class(ans) <- "summary.eglm"
  return(ans)
}
