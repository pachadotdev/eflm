fglm.wfit <- function(y, X, intercept = TRUE, weights = NULL,
                      family = gaussian(), start = NULL, etastart = NULL,
                      mustart = NULL, offset = NULL, maxit = 25, k = 2,
                      tol.estimation = tol.estimation, tol.values = tol.values,
                      tol.vectors = tol.vectors, tol.solve = tol.solve,
                      method = method, ...) {
  nobs <- NROW(y)
  nvar <- ncol(X)
  if (missing(y)) stop("Argument y is missing")
  if (missing(X)) stop("Argument X is missing")
  if (is.null(offset)) offset <- rep.int(0, nobs)
  if (is.null(weights)) weights <- rep(1, nobs)
  col.names <- dimnames(X)[[2]]
  method <- match.arg(method)
  fam <- family$family
  link <- family$link
  variance <- family$variance
  dev.resids <- family$dev.resids
  aic <- family$aic
  linkinv <- family$linkinv
  mu.eta <- family$mu.eta
  if (is.null(start)) {
    if (is.null(mustart)) eval(family$initialize)
    eta <- if (is.null(etastart)) family$linkfun(mustart) else etastart
    mu <- mustart
    start <- rep(0, nvar)
  } else {
    eta <- offset + as.vector(if (nvar == 1) {
      X * start
    } else {
      tcrossprod(X, t(start))
    })
    mu <- linkinv(eta)
  }
  iter <- 0
  dev <- sum(dev.resids(y, mu, weights))
  tol <- 1
  if ((fam == "gaussian") & (link == "identity")) maxit <- 1
  C_Cdqrls <- getNativeSymbolInfo("Cdqrls", PACKAGE = getLoadedDLLs()$stats)
  while ((tol > tol.estimation) & (iter < maxit)) {
    iter <- iter + 1
    beta <- start
    dev0 <- dev
    varmu <- variance(mu)
    mu.eta.val <- mu.eta(eta)
    z <- (eta - offset) + (y - mu) / mu.eta.val
    W <- (weights * mu.eta.val * mu.eta.val) / varmu
    XTX <- cp(X, W)
    XTz <- t(crossprod((W * z), X))
    if (iter == 1 & method != "qr") {
      variable <- colnames(X)
      ris <- control(XTX, , tol.values, tol.vectors, , method)
      ok <- ris$pivot[1:ris$rank]
      XTX <- ris$XTX
      X <- X[, ok]
      XTz <- XTz[ok]
      start <- start[ok]
      beta <- start
    }
    if (method == "qr") {
      ris <- .Call(C_Cdqrls, XTX, XTz, tol.values, FALSE)
      start <- if (ris$rank < nvar) {
        ris$coefficients[ris$pivot]
      } else {
        ris$coefficients
      }
    } else {
      start <- solve(XTX, XTz, tol = tol.solve)
    }
    eta <- drop(tcrossprod(X, t(start)))
    mu <- linkinv(eta <- eta + offset)
    dev <- sum(dev.resids(y, mu, weights))
    tol <- max(abs(dev0 - dev) / (abs(dev) + 0.1))
  }
  wt <- sum(weights)
  wtdmu <- if (intercept) sum(weights * y) / wt else linkinv(offset)
  nulldev <- sum(dev.resids(y, wtdmu, weights))
  n.ok <- nobs - sum(weights == 0)
  nulldf <- n.ok - as.integer(intercept)
  rank <- ris$rank
  dfr <- nobs - rank - sum(weights == 0)
  aic.model <- aic(y, nobs, mu, weights, dev) + k * rank
  ll.new <- ll.fglm(fam, aic.model, rank)
  res <- (y - mu) / mu.eta(eta)
  resdf <- n.ok - rank
  RSS <- sum(W * res * res)
  var_res <- RSS / dfr
  dispersion <- if (fam %in% c("poisson", "binomial")) 1 else var_res
  if (method == "qr") {
    coefficients <- start
    coefficients[coefficients == 0] <- NA
    ok <- ris$pivot[1:rank]
  }
  else {
    coefficients <- rep(NA, nvar)
    start <- as(start, "numeric")
    coefficients[ok] <- start
  }
  names(coefficients) <- col.names
  rval <- list(
    "coefficients" = coefficients,
    "residuals" = res,
    "logLik" = ll.new,
    "iter" = iter,
    "tol" = tol,
    "family" = family,
    "link" = link,
    "df.residual" = dfr,
    "XTX" = XTX,
    "dispersion" = dispersion,
    "ok" = ok,
    "rank" = rank,
    "RSS" = RSS,
    method = method,
    "aic" = aic.model,
    "deviance" = dev,
    "df.null" = nulldf,
    "null.deviance" = nulldev,
    "ngoodobs" = n.ok,
    "n" = nobs,
    "intercept" = intercept,
    "convergence" = (!(tol > tol.estimation))
  )
  class(rval) <- "fglm"
  return(rval)
}

shglm <- function(formula, family = gaussian(),
                  start = NULL, etastart = NULL, mustart = NULL, offset = NULL,
                  maxit = 25, k = 2, ...) {
  if (!is.null(start)) stop("Sorry, code for argument start is not implemented yet")
  if (!is.null(mustart)) stop("Sorry, code for argument mustart is not implemented yet")
  if (!is.null(etastart)) stop("Sorry, code for argument etastart is not implemented yet")
  call <- match.call()
  tf <- terms(formula, data = dati)
  M <- model.frame(tf, dati)
  y <- M[[1]]
  X <- model.matrix(tf, M)
  offset <- model.offset(M)
  obj <- list()
  obj$terms <- tf
  fa <- which(attributes(attributes(M)$terms)$dataClasses == "factor")
  if (length(fa) > 0) {
    for (i in 1:length(fa)) {
      eval(parse(text = paste("obj$levels$'", names(M)[fa[i]],
                              "'", "<-levels(M[,fa[i]])",
                              sep = ""
      )))
    }
  }
  nomicol <- colnames(dati)
  variable <- colnames(X)
  nobs <- length(y)
  weights <- rep(1, nobs)
  intercept <- attributes(tf)$intercept
  nvar <- ncol(X)
  if (is.null(offset)) {
    offset <- rep.int(0, nobs)
  }
  sp <- NULL
  ok <- 1:ncol(X)
  rank <- ncol(X)
  dfr <- nobs - rank - sum(weights == 0)
  fam <- family$family
  link <- family$link
  linkfun <- family$linkfun
  variance <- family$variance
  dev.resids <- family$dev.resids
  linkinv <- family$linkinv
  mu.eta <- family$mu.eta
  iter <- 0
  tol <- 1
  dev <- nobs
  weights.cum <- wt <- wy <- nulldev <- tot.obs <- zero.weights <- 0
  block.cum <- nrow(X)
  end.iter <- FALSE

  while ((tol > tol.estimation) & (iter < maxit)) {
    dev.prec <- dev
    aic.model <- dev <- Wy <- RSS <- 0
    iter <- iter + 1
    XTX <- matrix(0, ncol(X), ncol(X))
    XTz <- matrix(0, ncol(X), 1)
    eof <- FALSE
    iter2 <- 0
    while (!eof) {
      iter2 <- iter2 + 1
      if (iter > 1) {
        if (nvar == 1) {
          eta <- (offset + as.vector(X * start))[1:length(y)]
        } else {
          eta <- (offset + drop(tcrossprod(X[, ok], t(start))))[1:length(y)]
        }
        mu <- linkinv(eta <- eta)
      } else {
        if (is.null(mustart)) eval(family$initialize)
        if (is.null(start)) {
          eta <- (if (is.null(etastart)) linkfun(mustart) else etastart)[1:length(y)]
          mu <- mustart[1:length(y)]
        } else {
          if (nvar == 1) {
            eta <- (offset + as.vector(X * start))[1:length(y)]
          } else {
            eta <- (offset + as.vector(tcrossprod(X, t(start))))[1:length(y)]
          }
          mu <- linkinv(eta)
        }
      }
      dev <- dev + sum(dev.resids(y, mu, weights))
      if (iter > 1) aic.model <- aic.model + aic.shglm(fam, y, wt, mu, weights, dev.prec)
      varmu <- variance(mu)
      mu.eta.val <- mu.eta(eta)
      z <- (eta - offset) + (y - mu) / mu.eta.val
      W <- (weights * mu.eta.val * mu.eta.val) / varmu
      if (iter2 == 1) {
        XTX <- XTX + cp(X, W)
        XTz <- XTz + t(crossprod((W * z), X))
      } else {
        Ax <- XTX
        XTX <- cp(X, W)
        XTX[rownames(Ax), colnames(Ax)] <- XTX[rownames(Ax), colnames(Ax)] + Ax
        Az <- XTz
        XTz <- t(crossprod((W * z), X))
        XTz[rownames(Az), ] <- XTz[rownames(Az), ] + Az
      }
      res <- (y - mu) / mu.eta(eta)
      RSS <- RSS + sum(W * res * res)
      if (iter == 1) weights.cum <- weights.cum + sum(weights == 0)
      if (iter == 1) {
        ris <- control(XTX, , tol.values, tol.vectors, out.B = FALSE, method)
        ok <- ris$pivot[1:ris$rank]
      }
      if (is.null(start)) start <- rep(0, rank)
      beta <- if (iter == 1) start[ok] else start
      start <- solve(XTX[ok, ok], XTz[ok], tol = tol.solve)
      tol <- max(abs(dev.prec - dev) / (abs(dev) + 0.1))
      if ((tol > tol.estimation) & (iter < maxit)) {
        eof <- TRUE
      } else {
        break
      }
      colnames(dati) <- nomicol
      M <- model.frame(tf, dati)
      y <- M[[1]]
      if (length(fa) > 0) {
        flevels <- list()
        j <- 0
        for (i in 1:length(fa)) {
          j <- j + 1
          eval(parse(text = paste("flevels$'", names(M)[fa[i]],
                                  "'", "<-levels(M[,fa[i]])",
                                  sep = ""
          )))
          a <- c(obj$levels[[j]][!(obj$levels[[j]] %in% flevels[[j]])], flevels[[j]])
          flevels[[j]] <- sort(a)
        }
        M <- model.frame(obj$terms, dati, xlev = flevels)
        X <- model.matrix(obj$terms, M, xlev = flevels)
        obj$levels <- flevels
      } else {
        X <- model.matrix(obj$terms, M)
        flevels <- obj$levels
      }
      offset <- model.offset(M)
      nobs <- length(y)
      if (is.null(offset)) {
        offset <- rep.int(0, nobs)
      }
      weights <- rep(1, nobs)
      if (iter == 1) {
        tot.obs <- tot.obs + nobs
        wt <- wt + sum(weights)
        wy <- wy + crossprod(weights, y)
        zero.weights <- zero.weights + sum(weights == 0)
        block.cum <- block.cum + nrow(X)
      }
      if (iter == 2) {
        wtdmu <- if (intercept) wy / wt else linkinv(offset)
        nulldev <- nulldev + sum(dev.resids(y, wtdmu, weights))
      }
    }
  }
  rank <- ris$rank
  n.ok <- tot.obs - zero.weights
  nulldf <- n.ok - as.integer(intercept)
  aic.rest <- ifelse((fam %in% c(
    "Gamma", "inverse.gaussian",
    "gaussian"
  )), 2, 0)
  aic.model <- aic.model + k * rank + aic.rest
  ll.new <- ll.fglm(fam, aic.model, rank)
  resdf <- n.ok - rank
  var_res <- RSS / resdf
  dispersion <- if (fam %in% c("poisson", "binomial")) 1 else var_res
  coefficients <- rep(NA, ncol(X))
  start <- as(start, "numeric")
  coefficients[ok] <- start
  names(coefficients) <- colnames(X)
  rval <- list(
    coefficients = coefficients, logLik = ll.new,
    iter = iter, tol = tol, family = family, link = link, df = resdf,
    XTX = XTX[ok, ok], dispersion = dispersion, nok = ris$nok,
    ok = ok, RSS = RSS, ncoll = ris$ncoll,
    nulldev = nulldev, rank = rank, deviance = dev,
    nulldf = nulldf, ngoodobs = n.ok, n = tot.obs, intercept = intercept,
    aic = aic.model, convergence = (!(tol > tol.estimation)), method = "eigen"
  )
  rval$tf <- tf
  rval$call <- call
  if ((rval$iter == maxit) & (!rval$convergence)) {
    warning("Maximum number of iterations reached without convergence")
  }
  class(rval) <- "fglm"
  rval
}

control <- function(B, symmetric = TRUE, tol.values = 1e-07, tol.vectors = 1e-07,
                    out.B = TRUE, method = c("eigen", "Cholesky")) {
  method <- match.arg(method)
  if (!(method %in% c("eigen", "Cholesky"))) {
    stop("method not valid or not implemented")
  }
  if (method == "eigen") {
    n <- ncol(B)
    sa <- 1:n
    nok <- NULL
    auto <- eigen(B, symmetric, only.values = TRUE)
    totcoll <- sum(abs(auto$values) < tol.values)
    ncoll <- totcoll
    rank <- n - ncoll
    i <- 1
    while (ncoll != 0) {
      auto <- eigen(B, symmetric)
      j <- as.matrix(abs(auto$vectors[, n]) < tol.vectors)
      coll <- which(!j)
      coll <- coll[length(coll)]
      B <- B[-coll, -coll]
      nok[i] <- coll
      ncoll <- sum(abs(auto$values) < tol.values) - 1
      n <- ncol(B)
      i <- i + 1
    }
    ok <- if (!is.null(nok)) {
      sa[-nok]
    } else {
      sa
    }
  }
  if (method == "Cholesky") {
    A <- chol(B, pivot = TRUE)
    pivot <- attributes(A)$"pivot"
    rank <- attributes(A)$"rank"
    ok <- sort(pivot[1:rank])
    nok <- if (rank < length(pivot)) pivot[(rank + 1):length(pivot)] else NULL
    B <- B[ok, ok]
  }
  rval <- if (out.B) {
    list(XTX = B, rank = rank, pivot = c(ok, nok))
  } else {
    list(rank = rank, pivot = c(ok, nok))
  }

  rval
}

cp <- function(X, w = NULL) {
  new.B <- if (is.null(w)) {
    crossprod(X)
  } else {
    crossprod(sqrt(w) * X)
  }
  as(new.B, "matrix")
}

ll.fglm <- function(family, aic.model, nvar) {
  switch(family,
         binomial = -(aic.model - 2 * nvar) / 2,
         Gamma = -((aic.model - 2 * nvar) - 2) / 2,
         gaussian = -((aic.model - 2 * nvar) - 2) / 2,
         poisson = -(aic.model - 2 * nvar) / 2,
         inverse.gaussian = -((aic.model - 2 * nvar) - 2) / 2,
         quasi = NA,
         quasibinomial = NA,
         quasipoisson = NA
  )
}

aic.Gamma <- function(y, n, mu, wt, dev) {
  disp <- dev / n
  -2 * sum(dgamma(y, 1 / disp, scale = mu * disp, log = TRUE) * wt) #+2
}

aic.binomial <- function(y, n, mu, wt, dev) {
  m <- if (any(n > 1)) n else wt
  -2 * sum(ifelse(m > 0, (wt / m), 0) * dbinom(round(m * y),
                                               round(m), mu,
                                               log = TRUE
  ))
}

aic.inverse.gaussian <- function(y, n, mu, wt, dev) {
  sum(wt) * (log(dev / sum(wt) * 2 * pi) + 1) + 3 * sum(log(y) * wt)
}

aic.poisson <- function(y, n, mu, wt, dev) {
  -2 * sum(dpois(y, mu, log = TRUE) * wt)
}

aic.gaussian <- function(y, n, mu, wt, dev) {
  n.obs <- length(y)
  n.obs * (log(dev / n.obs * 2 * pi) + 1) - sum(log(wt))
}

# the same as stats:::safe_pchisq
safe_pchisq <- function(q, df, ...)
{
  df[df <= 0] <- NA
  pchisq(q = q, df = df, ...)
}

# the same as stats:::safe_pchisq
safe_pf <- function(q, df1, ...)
{
  df1[df1 <= 0] <- NA
  pf(q = q, df1 = df1, ...)
}

aic.shglm <- function(family, y, n, mu, wt, dev) {
  options(warn = -1)
  a <- switch(family,
              binomial = aic.binomial(y, n, mu, wt, dev),
              Gamma = aic.Gamma(y, n, mu, wt, dev),
              gaussian = aic.gaussian(y, n, mu, wt, dev),
              poisson = aic.poisson(y, n, mu, wt, dev),
              inverse.gaussian = aic.inverse.gaussian(y, n, mu, wt, dev),
              quasi = NA,
              quasibinomial = NA,
              quasipoisson = NA
  )
  options(warn = 1)
  return(a)
}

print.logLik.fglm <- function(x, digits = getOption("digits"), ...) {
  cat("'log Lik.' ", paste(format(logLik(x), digits = digits), collapse = ", "),
    " (df=", format(attr(x, "df")), ")\n",
    sep = ""
  )
  invisible(x)
}

logLik.fglm <- function(object, ...) {
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

coef.fglm <- function(object, ...) object$coefficients

vcov.fglm <- function(object, ...) object$dispersion * solve(object$XTX)

deviance.fglm <- function(object, ...) object$deviance

AIC.fglm <- function(object, ...) {
  if (!(length(list(...)))) {
    object$aic
  } else {
    aic <- function(x) x$aic
    object <- list(object, ...)
    val <- sapply(object, aic)
    val
  }
}

extractAIC.fglm <- function(fit, scale = 0, k = 2, ...) {
  n <- fit$n
  edf <- n - fit$df
  aic <- fit$aic
  c(edf, aic + (k - 2) * edf)
}

drop1.fglm <- function(object, scope, scale = 0, test = c(
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
    z <- fglm.wfit(y, x[, jj, drop = FALSE],
      intercept = T, wt, offset = object$offset,
      family = object$family
    )
    dfs[i] <- z$rank
    dev[i] <- z$deviance
    if (test == "Rao") {
      r <- object$y - predict(object, newdata = object$model, type = "response")
      w <- weights
      zz <- fglm.wfit(r, x, TRUE, w, offset = object$offset)
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

add1.fglm <- function(object, scope, scale = 0, test = c(
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
  z <- fglm.wfit(y, X, , wt, offset = offset, family = object$family)
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
    z <- fglm.wfit(y, X, , wt, offset = offset, family = object$family)
    dfs[tt] <- z$rank
    dev[tt] <- z$deviance
    if (test == "Rao") {
      zz <- fglm.wfit(r, X, , w, offset = offset)
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

nobs.fglm <- function(object, use.fallback = FALSE, ...) {
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

predict.flm <- function (object, newdata, na.action = na.pass, ...) {
  tt <- terms(object)
  if (!inherits(object, c("flm","fglm")))
    warning("calling predict.flm(<fake-flm/fglm-object>) ...")
  if (missing(newdata) || is.null(newdata)) {
    if(is.null(object$fitted.values))
      warning("Fitted values were not returned from the fglm object:
              use the original data by setting argument 'newdata' or refit
              the model by specifying fitted=TRUE.")
    return(object$fitted.values)
  }
  else {
    Terms <- delete.response(tt)
    m <- model.frame(Terms, newdata, na.action = na.action)#, xlev = object$xlevels)
    if (!is.null(cl <- attr(Terms, "dataClasses")))
      .checkMFClasses(cl, m)
    X <- model.matrix(Terms, m, contrasts.arg = object$contrasts)
    offset <- rep(0, nrow(X))
    if (!is.null(off.num <- attr(tt, "offset")))
      for (i in off.num) offset <- offset + eval(attr(tt,
                                                      "variables")[[i + 1]], newdata)
    if (!is.null(object$call$offset))
      offset <- offset + eval(object$call$offset, newdata)
  }
  p <- object$rank
  ord <- colnames(X)
  if (p < ncol(X) && !(missing(newdata) || is.null(newdata)))
    warning("Prediction from a rank-deficient fit may be misleading.")
  beta <- object$coefficients
  beta[is.na(beta)] <- 0
  predictor <- drop(X[, ord, drop = FALSE] %*% beta[ord])
  if (!is.null(offset))
    predictor <- predictor + offset
  if (missing(newdata) && !is.null(na.act <- object$na.action))
    predictor <- napredict(na.act, predictor)
  predictor
}
