#' @export
#' @keywords internal
summary.fast_glm <- function(object, ...) {
  if (!inherits(object, "fast_glm")) {
    stop("object is not of class fast_glm")
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
    cov.unscaled = inv, cov.scaled = inv * var_res
  ))
  class(ans) <- "summary.fast_glm"
  return(ans)
}

#' @export
#' @keywords internal
print.summary.fast_glm <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  if (!is.null(x$call)) {
    cat("\nCall:\n")
    cat(deparse(x$call))
    cat("\n\n")
  }
  cat("Deviance residuals:\n")
  if (x$df.residual > 5) {
    x$deviance.resid <- setNames(quantile(x$deviance.resid,
      na.rm = TRUE), c("Min", "1Q", "Median", "3Q", "Max"))
  }
  xx <- zapsmall(x$deviance.resid, digits + 1L)
  print.default(xx, digits = digits, na.print = "", print.gap = 2L)
  cat("\n")
  if (length(x$coef) > 0) {
    cat("Coefficients:\n")
    sig <- function(z) {
      if (!is.na(z)) {
        if (z < 0.001) {
          "***"
        } else if (z < 0.01) {
          "** "
        } else if (z < 0.05) {
          "*  "
        } else if (z < 0.1) {
          ".  "
        } else {
          "   "
        }
      } else {
        "   "
      }
    }
    options(warn = -1)
    sig.1 <- sapply(
      as.numeric(as.character(x$coefficients[, 4])),
      sig
    )
    options(warn = 0)
    est.1 <- cbind(
      format(x$coefficients, digits = digits),
      sig.1
    )
    colnames(est.1)[ncol(est.1)] <- ""
    print(est.1)
    cat("---")
    cat("\n")
    cat(
      "Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
      "\n"
    )
    cat("\n")
  }
  else {
    cat("No coefficients\n")
  }
  cat("(Dispersion parameter for ", x$family$family, " family taken to be ",
      format(x$dispersion), ")\n\n", apply(cbind(paste(format(
        c("Null", "Residual"), justify = "right"), "deviance:"),
        format(unlist(x[c("null.deviance", "deviance")]), digits = max(5L, digits + 1L)), " on",
        format(unlist(x[c("df.null", "df.residual")])),
        " degrees of freedom\n"), 1L, paste, collapse = " "
        ), sep = "")

  cat("AIC:", format(x$aic, digits = max(4L, digits + 1L)))
  cat("\n\n")
  cat("Number of Fisher Scoring iterations:", x$iter)
  invisible(x)
}

#' @export
#' @keywords internal
print.fast_glm <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("\nCall:  ", paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  if (length(coef(x))) {
    cat("Coefficients")
    if (is.character(co <- x$contrasts))
      cat("  [contrasts: ", apply(cbind(names(co), co),
                                  1L, paste, collapse = "="), "]")
    cat(":\n")
    print.default(format(x$coefficients, digits = digits),
                  print.gap = 2, quote = FALSE)
  }
  else cat("No coefficients\n\n")
  cat("\nDegrees of Freedom:", x$df.null, "Total (i.e. Null); ",
    x$df.residual, "Residual\n")
  if (nzchar(mess <- naprint(x$na.action)))
    cat("  (", mess, ")\n", sep = "")
  cat("Null Deviance:\t   ", format(signif(x$null.deviance,
    digits)), "\nResidual Deviance:", format(signif(x$deviance,digits)),
    "\tAIC:", format(signif(x$aic, digits)))
  cat("\n")
  invisible(x)
}

print.logLik.fast_glm <- function(x, digits = getOption("digits"), ...) {
  cat("'log Lik.' ", paste(format(logLik(x), digits = digits), collapse = ", "),
    " (df=", format(attr(x, "df")), ")\n",
    sep = ""
  )
  invisible(x)
}

logLik.fast_glm <- function(object, ...) {
  if (!missing(...)) {
    warning("extra arguments discarded")
  }
  fam <- family(object)$family
  p <- object$rank
  if (fam %in% c("gaussian", "Gamma", "inverse.gaussian")) {
    p <- p + 1
  }
  val <- p - object$aic / 2
  attr(val, "nobs") <- object$n
  attr(val, "df") <- p
  class(val) <- "logLik"
  val
}

coef.fast_glm <- function(object, ...) object$coefficients

vcov.fast_glm <- function(object, ...) object$dispersion * solve(object$XTX)

deviance.fast_glm <- function(object, ...) object$deviance

AIC.fast_glm <- function(object, ...) {
  if (!(length(list(...)))) {
    object$aic
  } else {
    aic <- function(x) x$aic
    object <- list(object, ...)
    val <- sapply(object, aic)
    val
  }
}

extractAIC.fast_glm <- function(fit, scale = 0, k = 2, ...) {
  n <- fit$n
  edf <- n - fit$df
  aic <- fit$aic
  c(edf, aic + (k - 2) * edf)
}

drop1.fast_glm <- function(object, scope, scale = 0, test = c(
                             "none", "Rao", "LRT",
                             "Chisq", "F"
                           ), k = 2, weights = rep(1, object$n), ...) {
  if (is.null(object$model)) stop("object must be fitted with options model=TRUE, y=TRUE and fitted=TRUE")
  test <- match.arg(test)
  if (test == "Chisq") {
    test <- "LRT"
  }
  x <- model.matrix(formula(object), object$model)
  n <- nrow(x)
  asgn <- attr(x, "assign")
  tl <- attr(object$terms, "term.labels")
  if (missing(scope)) {
    scope <- drop.scope(object)
  } else {
    if (!is.character(scope)) {
      scope <- attr(
        terms(update.formula(object, scope)),
        "term.labels"
      )
    }
    if (!all(match(scope, tl, 0L) > 0L)) {
      stop("scope is not a subset of term labels")
    }
  }
  ndrop <- match(scope, tl)
  ns <- length(scope)
  rdf <- object$df
  chisq <- object$deviance
  dfs <- numeric(ns)
  dev <- numeric(ns)
  score <- numeric(ns)
  y <- object$y
  if (is.null(y)) {
    y <- model.response(model.frame(object))
    if (!is.factor(y)) {
      storage.mode(y) <- "double"
    }
  }
  wt <- weights
  if (is.null(wt)) {
    wt <- rep.int(1, n)
  }
  for (i in seq_len(ns)) {
    ii <- seq_along(asgn)[asgn == ndrop[i]]
    jj <- setdiff(seq(ncol(x)), ii)
    z <- fast_glm.wfit(y, x[, jj, drop = FALSE],
      intercept = T, wt, offset = object$offset,
      family = object$family
    )
    dfs[i] <- z$rank
    dev[i] <- z$deviance
    if (test == "Rao") {
      r <- object$y - predict(object, newdata = object$model, type = "response")
      w <- weights
      zz <- fast_glm.wfit(r, x, TRUE, w, offset = object$offset)
      score[i] <- zz$null.deviance - zz$deviance
    }
  }
  scope <- c("<none>", scope)
  dfs <- c(object$rank, dfs)
  dev <- c(chisq, dev)
  if (test == "Rao") {
    score <- c(NA, score)
  }
  dispersion <- if (is.null(scale) || scale == 0) {
    summary(object, dispersion = NULL)$dispersion
  } else {
    scale
  }
  fam <- object$family$family
  loglik <- if (fam == "gaussian") {
    if (scale > 0) {
      dev / scale - n
    } else {
      n * log(dev / n)
    }
  }
  else {
    dev / dispersion
  }
  aic <- loglik + k * dfs
  dfs <- dfs[1L] - dfs
  dfs[1L] <- NA
  aic <- aic + (extractAIC(object, k = k)[2L] - aic[1L])
  aod <- data.frame(
    Df = dfs, Deviance = dev, AIC = aic, row.names = scope,
    check.names = FALSE
  )
  if (all(is.na(aic))) {
    aod <- aod[, -3]
  }
  if (test == "LRT") {
    dev <- pmax(0, loglik - loglik[1L])
    dev[1L] <- NA
    nas <- !is.na(dev)
    LRT <- if (dispersion == 1) {
      "LRT"
    } else {
      "scaled dev."
    }
    aod[, LRT] <- dev
    dev[nas] <- safe_pchisq(dev[nas], aod$Df[nas], lower.tail = FALSE)
    aod[, "Pr(>Chi)"] <- dev
  }
  else if (test == "Rao") {
    dev <- pmax(0, score)
    nas <- !is.na(dev)
    SC <- if (dispersion == 1) {
      "Rao score"
    } else {
      "scaled Rao sc."
    }
    dev <- dev / dispersion
    aod[, SC] <- dev
    dev[nas] <- safe_pchisq(dev[nas], aod$Df[nas], lower.tail = FALSE)
    aod[, "Pr(>Chi)"] <- dev
  }
  else if (test == "F") {
    if (fam == "binomial" || fam == "poisson") {
      warning(gettextf(
        "F test assumes 'quasi%s' family",
        fam
      ), domain = NA)
    }
    dev <- aod$Deviance
    rms <- dev[1L] / rdf
    dev <- pmax(0, dev - dev[1L])
    dfs <- aod$Df
    rdf <- object$df
    Fs <- (dev / dfs) / rms
    Fs[dfs < 1e-04] <- NA
    P <- Fs
    nas <- !is.na(Fs)
    P[nas] <- safe_pf(Fs[nas], dfs[nas], rdf, lower.tail = FALSE)
    aod[, c("F value", "Pr(>F)")] <- list(Fs, P)
  }
  head <- c(
    "Single term deletions", "\nModel:", deparse(formula(object)),
    if (!is.null(scale) && scale > 0) {
      paste(
        "\nscale: ",
        format(scale), "\n"
      )
    }
  )
  class(aod) <- c("anova", "data.frame")
  attr(aod, "heading") <- head
  aod
}

add1.fast_glm <- function(object, scope, scale = 0, test = c(
                            "none", "Rao", "LRT",
                            "Chisq", "F"
                          ), x = NULL, k = 2, weights = rep(1, object$n), ...) {
  if (is.null(object$model)) stop("object must be fitted with options model=TRUE, y=TRUE and fitted=TRUE")
  Fstat <- function(table, rdf) {
    dev <- table$Deviance
    df <- table$Df
    diff <- pmax(0, (dev[1L] - dev) / df)
    Fs <- diff / (dev / (rdf - df))
    Fs[df < .Machine$double.eps] <- NA
    P <- Fs
    nnas <- !is.na(Fs)
    P[nnas] <- safe_pf(Fs[nnas], df[nnas], rdf - df[nnas],
      lower.tail = FALSE
    )
    list(Fs = Fs, P = P)
  }
  test <- match.arg(test)
  if (test == "Chisq") {
    test <- "LRT"
  }
  if (!is.character(scope)) {
    scope <- add.scope(object, update.formula(object, scope))
  }
  if (!length(scope)) {
    stop("no terms in scope for adding to object")
  }
  oTerms <- attr(object$terms, "term.labels")
  int <- attr(object$terms, "intercept")
  ns <- length(scope)
  dfs <- dev <- score <- numeric(ns + 1)
  names(dfs) <- names(dev) <- names(score) <- c("<none>", scope)
  add.rhs <- paste(scope, collapse = "+")
  add.rhs <- eval(parse(text = paste("~ . +", add.rhs), keep.source = FALSE))
  new.form <- update.formula(object, add.rhs)
  Terms <- terms(new.form)
  y <- object$y
  if (is.null(x)) {
    fc <- object$call
    fc$formula <- Terms
    fob <- list(call = fc, terms = Terms)
    class(fob) <- oldClass(object)
    m <- model.frame(fob)
    offset <- model.offset(m)
    wt <- weights
    x <- model.matrix(Terms, m, contrasts.arg = object$contrasts)
    oldn <- length(y)
    y <- model.response(m)
    if (!is.factor(y)) {
      storage.mode(y) <- "double"
    }
    if (NCOL(y) == 2) {
      n <- y[, 1] + y[, 2]
      y <- ifelse(n == 0, 0, y[, 1] / n)
      if (is.null(wt)) {
        wt <- rep.int(1, length(y))
      }
      wt <- wt * n
    }
    newn <- length(y)
    if (newn < oldn) {
      warning(sprintf(
        ngettext(
          newn, "using the %d/%d row from a combined fit",
          "using the %d/%d rows from a combined fit"
        ),
        newn, oldn
      ), domain = NA)
    }
  }
  else {
    wt <- object$prior.weights
    offset <- object$offset
  }
  n <- nrow(x)
  if (is.null(wt)) {
    wt <- rep.int(1, n)
  }
  Terms <- attr(Terms, "term.labels")
  asgn <- attr(x, "assign")
  ousex <- match(asgn, match(oTerms, Terms), 0L) > 0L
  if (int) {
    ousex[1L] <- TRUE
  }
  X <- x[, ousex, drop = FALSE]
  z <- fast_glm.wfit(y, X, , wt, offset = offset, family = object$family)
  dfs[1L] <- z$rank
  dev[1L] <- z$deviance
  r <- z$residuals
  w <- z$weights
  sTerms <- sapply(strsplit(Terms, ":", fixed = TRUE), function(x) {
    paste(sort(x),
      collapse = ":"
    )
  })
  for (tt in scope) {
    stt <- paste(sort(strsplit(tt, ":")[[1L]]), collapse = ":")
    usex <- match(asgn, match(stt, sTerms), 0L) > 0L
    X <- x[, usex | ousex, drop = FALSE]
    z <- fast_glm.wfit(y, X, , wt, offset = offset, family = object$family)
    dfs[tt] <- z$rank
    dev[tt] <- z$deviance
    if (test == "Rao") {
      zz <- fast_glm.wfit(r, X, , w, offset = offset)
      score[tt] <- zz$null.deviance - zz$deviance
    }
  }
  if (scale == 0) {
    dispersion <- summary(object, dispersion = NULL)$dispersion
  } else {
    dispersion <- scale
  }
  fam <- object$family$family
  if (fam == "gaussian") {
    if (scale > 0) {
      loglik <- dev / scale - n
    } else {
      loglik <- n * log(dev / n)
    }
  }
  else {
    loglik <- dev / dispersion
  }
  aic <- loglik + k * dfs
  aic <- aic + (extractAIC(object, k = k)[2L] - aic[1L])
  dfs <- dfs - dfs[1L]
  dfs[1L] <- NA
  aod <- data.frame(
    Df = dfs, Deviance = dev, AIC = aic, row.names = names(dfs),
    check.names = FALSE
  )
  if (all(is.na(aic))) {
    aod <- aod[, -3]
  }
  test <- match.arg(test)
  if (test == "LRT") {
    dev <- pmax(0, loglik[1L] - loglik)
    dev[1L] <- NA
    LRT <- if (dispersion == 1) {
      "LRT"
    } else {
      "scaled dev."
    }
    aod[, LRT] <- dev
    nas <- !is.na(dev)
    dev[nas] <- safe_pchisq(dev[nas], aod$Df[nas], lower.tail = FALSE)
    aod[, "Pr(>Chi)"] <- dev
  }
  else if (test == "Rao") {
    dev <- pmax(0, score)
    dev[1L] <- NA
    nas <- !is.na(dev)
    SC <- if (dispersion == 1) {
      "Rao score"
    } else {
      "scaled Rao sc."
    }
    dev <- dev / dispersion
    aod[, SC] <- dev
    dev[nas] <- safe_pchisq(dev[nas], aod$Df[nas], lower.tail = FALSE)
    aod[, "Pr(>Chi)"] <- dev
  }
  else if (test == "F") {
    if (fam == "binomial" || fam == "poisson") {
      warning(gettextf(
        "F test assumes quasi%s family",
        fam
      ), domain = NA)
    }
    rdf <- object$df.residual
    aod[, c("F value", "Pr(>F)")] <- Fstat(aod, rdf)
  }
  head <- c(
    "Single term additions", "\nModel:", deparse(formula(object)),
    if (scale > 0) paste("\nscale: ", format(scale), "\n")
  )
  class(aod) <- c("anova", "data.frame")
  attr(aod, "heading") <- head
  aod
}

nobs.fast_glm <- function(object, use.fallback = FALSE, ...) {
  if (!is.null(w <- object$weights)) sum(w != 0) else object$n
}

estfun.glm <- function(x, ...)
{
  xmat <- model.matrix(x)
  xmat <- naresid(x$na.action, xmat)
  if(any(alias <- is.na(coef(x)))) xmat <- xmat[, !alias, drop = FALSE]
  wres <- as.vector(residuals(x, "working")) * weights(x, "working")
  dispersion <- if(substr(x$family$family, 1, 17) %in% c("poisson", "binomial", "Negative Binomial")) 1
  else sum(wres^2, na.rm = TRUE)/sum(weights(x, "working"), na.rm = TRUE)
  rval <- wres * xmat / dispersion
  attr(rval, "assign") <- NULL
  attr(rval, "contrasts") <- NULL
  res <- residuals(x, type = "pearson")
  if(is.ts(res)) rval <- ts(rval, start = start(res), frequency = frequency(res))
  if(is.zoo(res)) rval <- zoo(rval, index(res), attr(res, "frequency"))
  return(rval)
}
