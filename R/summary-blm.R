#' Summarizing Linear Model Fits
#'
#' This function is a method for class blm objects.
#'
#' @param object an object of class "blm", usually, a result of a call to blm
#' @param \dots further arguments passed to or from other methods
#' @importFrom stats pt
#' @export
summary.blm <- function(object, ...) {
  if (!inherits(object, "blm")) {
    stop("object must be an object of class blm")
  }
  z <- object
  n <- if (is.null(z$weights)) z$nobs else z$nobs - z$zero.w
  nvar <- z$nvar
  rdf <- z$df.residual
  if (z$singularity.method == "qr") {
    z$XTX <- z$XTX[z$ok, z$ok]
  }
  var_res <- as.numeric(z$RSS) / rdf
  se_coef <- rep(NA, z$nvar)
  inv <- solve.qr(qr(z$XTX, LAPACK = T))
  colnames(inv) <- rownames(inv)
  se_coef[z$ok] <- sqrt(var_res * diag(inv))
  t1 <- z$coefficients / se_coef
  p <- 2 * pt(abs(t1), df = z$df.residual, lower.tail = FALSE)
  ip <- !is.na(p)
  if (is.null(z$weights)) {
    X1X <- z$X1X[z$ok]
    if (z$intercept) {
      X1X <- matrix(kronecker(X1X, X1X), z$rank, z$rank)
      mss <- crossprod(z$coef, z$XTX - X1X / n) %*% z$coef
    } else {
      mss <- crossprod(z$coef, z$XTX) %*% z$coef
    }
  } else {
    XW1 <- z$XW1[z$ok]
    mss <- if (z$intercept) {
      XWX <- matrix(kronecker(XW1, XW1), length(XW1), length(XW1))
      XW1 <- matrix(kronecker(XW1 * z$SW, XW1), z$rank, z$rank, byrow = TRUE)
      crossprod(z$coef, z$XTX - 2 * XWX / z$SW + XW1 / (z$SW^2)) %*% z$coef
    } else {
      crossprod(z$coef, z$XTX) %*% z$coef
    }
  }
  rss <- z$RSS
  if (nvar != (z$intercept)) {
    df.int <- if (z$intercept) 1L else 0L
    r.squared <- as.numeric(mss / (mss + rss))
    adj.r.squared <- 1 - (1 - r.squared) * ((n - df.int) / rdf)
    fstatistic <- c(value = (as(mss, "numeric") /
                               (z$rank - df.int)) / var_res, numdf = z$rank - df.int, dendf = rdf)
    f.pvalue <- 1 - pf(fstatistic[1], fstatistic[2], fstatistic[3])
  }
  else {
    fstatistic <- f.pvalue <- NULL
    r.squared <- adj.r.squared <- 0
  }
  param <- data.frame(
    coef = z$coefficients, se = se_coef,
    t = t1, `p-value` = p
  )
  dimnames(param) <- list(
    names(z$coefficients),
    c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  )
  keep <- match(
    c("call", "terms", "frame", "ok", "RSS", "rank"),
    names(object), 0
  )
  ans <- c(object[keep], list(
    coefficients = param,
    residuals = z$residuals,
    var.res = var_res,
    df.residuals = z$df.residual,
    nobs = z$nobs,
    r.squared = r.squared,
    adj.r.squared = adj.r.squared,
    fstatistic = fstatistic,
    f.pvalue = f.pvalue,
    rdf = rdf,
    cov.unscaled = inv,
    intercept = (nvar != (z$intercept))
  ))

  class(ans) <- "summary.blm"
  return(ans)
}
