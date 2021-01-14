#' Summarizing Generalized Linear Model Fits
#'
#' This function is a method for class fglm objects.
#'
#' @param object an object of class "fglm", usually, a result of a call to fglm
#' @param \dots further arguments passed to or from other methods
#' @importFrom stats pnorm pt na.omit
#' @export
summary.fglm <- function(object, ...) {
  if (!inherits(object, "fglm")) {
    stop("object is not of class fglm")
  }
  z <- object
  var_res <- as.numeric(z$RSS / z$df.residual)
  dispersion <- if (z$family$family %in% c("poisson", "binomial")) 1 else var_res
  if (z$singularity.method == "qr") {
    z$XTX <- z$XTX[z$ok, z$ok]
  }
  inv <- solve(z$XTX, tol = z$tol.solve)
  covmat <- diag(inv)
  se_coef <- rep(NA, length(z$coefficients))
  se_coef[z$ok] <- sqrt(dispersion * covmat)
  if (z$family$family %in% c("binomial", "poisson")) {
    z1 <- z$coefficients / se_coef
    p <- 2 * pnorm(abs(z1), lower.tail = FALSE)
  } else {
    t1 <- z$coefficients / se_coef
    p <- 2 * pt(abs(t1), df = z$df.residual, lower.tail = FALSE)
  }
  ip <- !is.na(p)
  p[ip] <- as.numeric(format(p[ip], digits = 3))
  dn <- c("Estimate", "Std. Error")
  if (z$family$family %in% c("binomial", "poisson")) {
    format.coef <- if (any(na.omit(abs(z$coef)) < 1e-04)) {
      format(z$coefficients, scientific = TRUE, digits = 4)
    } else {
      round(z$coefficients, digits = 7)
    }
    format.se <- if (any(na.omit(se_coef) < 1e-04)) {
      format(se_coef, scientific = TRUE, digits = 4)
    } else {
      round(se_coef, digits = 7)
    }
    format.pv <- if (any(na.omit(p) < 1e-04)) {
      format(p, scientific = TRUE, digits = 4)
    } else {
      round(p, digits = 4)
    }
    param <- data.frame(format.coef, format.se, round(z1,
      digits = 4
    ), format.pv)
    dimnames(param) <- list(names(z$coefficients), c(
      dn,
      "z value", "Pr(>|z|)"
    ))
  } else {
    format.coef <- if (any(abs(na.omit(z$coefficients)) <
      1e-04)) {
      format(z$coefficients, scientific = TRUE, digits = 4)
    } else {
      round(z$coefficients, digits = 7)
    }
    format.se <- if (any(na.omit(se_coef) < 1e-04)) {
      format(se_coef, scientific = TRUE, digits = 4)
    } else {
      round(se_coef, digits = 7)
    }
    format.pv <- if (any(na.omit(p) < 1e-04)) {
      format(p, scientific = TRUE, digits = 4)
    } else {
      round(p, digits = 4)
    }
    param <- data.frame(format.coef, format.se, round(t1,
      digits = 4
    ), format.pv)
    dimnames(param) <- list(names(z$coefficients), c(
      dn,
      "t value", "Pr(>|t|)"
    ))
  }
  eps <- 10 * .Machine$double.eps
  if (z$family$family == "binomial") {
    if (any(z$mu > 1 - eps) || any(z$mu < eps)) {
      warning("fitted probabilities numerically 0 or 1 occurred")
    }
  }
  if (z$family$family == "poisson") {
    if (any(z$mu < eps)) {
      warning("fitted rates numerically 0 occurred")
    }
  }
  deviance.resid <- stats::residuals(z, type = "deviance")
  keep <- match(
    c(
      "call", "terms", "family", "deviance", "deviance.resid", "aic",
      "df.residual", "null.deviance", "df.null", "iter", "tol", "n", "convergence",
      "ngoodobs", "logLik", "RSS", "rank"
    ), names(object),
    0
  )
  ans <- c(object[keep], list(
    deviance.resid = deviance.resid,
    coefficients = param, dispersion = dispersion,
    cov.unscaled = inv, cov.scaled = inv * dispersion
  ))
  class(ans) <- "summary.fglm"
  return(ans)
}

#' Summarizing Linear Model Fits
#'
#' This function is a method for class flm objects.
#'
#' @param object an object of class "flm", usually, a result of a call to flm
#' @param \dots further arguments passed to or from other methods
#' @importFrom stats pt
#' @export
summary.flm <- function(object, ...) {
  if (!inherits(object, "flm")) {
    stop("object must be an object of class flm")
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
  # dimnames(param) <- list(
  #   names(z$coefficients),
  #   c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  # )
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
    cov.scaled = inv,
    intercept = (nvar != (z$intercept))
  ))

  class(ans) <- "summary.flm"
  return(ans)
}
