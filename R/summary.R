#' Summarizing Generalized Linear Model Fits
#'
#' This function is a method for class fglm objects.
#'
#' @param object an object of class "fglm", usually, a result of a call to fglm
#' @param \dots further arguments passed to or from other methods
#' @export
summary.fglm <- function(object, ...) {
  if (!inherits(object, "fglm")) {
    stop("object is not of class fglm")
  }
  z <- object
  var_res <- as.numeric(z$RSS / z$df.residual)
  dispersion <- if (z$family$family %in% c("poisson", "binomial")) 1 else var_res
  if (z$method == "qr") {
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
