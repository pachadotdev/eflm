#' Generalized Linear Models for Large Data Sets
#'
#' Fits GLMs to large datasets as an alternativa to \code{glm()}. This function
#' is only efficient (and relevant) when the number of observations is much
#' larger than the number of parameters to estimate (\code{N >> P}), as it
#' reduces the \code{N x P} model matrix to a \code{P x P} matrix. Otherwise, if
#' you have \code{N == p}, there is no effective reduction. Computationally the
#' best performance is obtained when R is linked against OpenBLAS, Intel MKL or
#' other optimized BLAS library.
#'
#' @param formula A formula for the model
#' @param data A tibble or data.frame
#' @param family See the function \link{glm}, but here it must be specified with brackets (e.g. \code{quasipoisson()})
#' @param intercept Logical value to determine whearead to included an intercept in the null model (Defaults to \code{TRUE})
#' @param weights An optional vector of ‘prior weights’ to be used in the fitting process. Should be \code{NULL}
#' or a numeric vector (e.g. \code{data$weights}, defaults to \code{NULL})
#' @param start Starting values for the parameters in the linear predictor
#' @param etastart Starting values for the linear predictor
#' @param mustart Starting values for the vector of means
#' @param offset This can be used to specify an a priori known component to be included in the
#' linear predictor during fitting. This should be \code{NULL} or a numeric vector of length
#' equal to the number of cases. One or more offset terms can be included in the formula instead
#' or as well, and if more than one is specified their sum is used. See \code{stats::model.offset()}
#' @param maxit See the function \link{glm}
#' @param k The penalty per parameter to be used, the default \code{k = 2}
#' is the classical AIC
#' @param model A logical value indicating whether model frame should be included as a
#' component of the returned value (Defaults to \code{TRUE})
#' @param method The chosen method to detect for singularity. Defaults to \code{"eigen"} but
#' it can also be \code{"Cholesky"} or \code{"qr"}
#' @param x Logical value indicating whether the model matrix used in the fitting process
#' should be returned as components of the returned value (Defaults to \code{FALSE}, see the function \link{glm.fit})
#' @param y Logical value indicating whether the response vector used in the fitting process
#' should be returned as components of the returned value (Defaults to \code{FALSE}, see the function \link{glm.fit})
#' @param fitted.values Logical value indicating whether the fitted values (i.e. response prediction) will be returned (Defaults to \code{TRUE})
#' @param linear.predictors Logical value indicating whether the linear predictors (i.e. link prediction) will be returned (Defaults to \code{TRUE})
#' @param tol.estimation Tolerance to be used for the estimation (Defaults to 1e-8)
#' @param tol.solve (Defaults to \code{.Machine$double.eps}, see the function \link{solve()})
#' @param tol.values Tolerance to consider eigenvalues equal to zero (Defaults to 1e-7, see the function \link{control()}),
#' @param tol.vectors Tolerance to consider eigenvectors equal to zero (Defaults to 1e-7, see the function \link{control()})
#' @param \dots Additional optional arguments (See the function \link{glm})
#' @export

yotov_fast_glm <- function(formula, data, family = gaussian(), weights = NULL,
                           start = NULL, etastart = NULL, mustart = NULL, offset = NULL, maxit = 25, k = 2,
                           model = FALSE, method = "eigen",
                           x = FALSE, y = FALSE, fitted.values = TRUE, linear.predictors = TRUE,
                           tol.estimation = 1e-8, tol.solve = .Machine$double.eps,
                           tol.values = 1e-7, tol.vectors = 1e-7, ...) {
  call <- match.call()
  target <- y
  M <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset"), names(M), 0L)
  M <- M[c(1L, m)]
  M$drop.unused.levels <- TRUE
  M[[1L]] <- quote(stats::model.frame)
  M <- eval(M, parent.frame())
  y <- M[[1]]
  tf <- attr(M, "terms")
  X <- model.matrix(tf, M)
  offset <- model.offset(M)
  intercept <- attributes(tf)$intercept
  method <- match.arg(method)
  rval <- fast_glm.wfit(
    X = X,
    y = y,
    family = family,
    weights = weights,
    start = start,
    etastart = etastart,
    mustart = mustart,
    offset = offset,
    intercept = intercept,
    maxit = maxit,
    k = k,
    tol.estimation = tol.estimation,
    tol.solve = tol.solve,
    tol.values = tol.values,
    tol.vectors = tol.vectors,
    method = method
  )
  rval$terms <- tf
  rval$call <- call
  class(rval) <- c("fast_glm", "fast_lm")
  if (model) rval$model <- M
  if (x) rval$x <- X
  if (target) rval$y <- y
  if (fitted.values) rval$fitted.values <- predict.fast_glm(rval, newdata = M, type = "response")
  if (linear.predictors) rval$linear.predictors <- predict.fast_glm(rval, newdata = M, type = "link")
  if ((rval$iter == maxit) & (!rval$convergence)) {
    warning("Maximum number of iterations reached without convergence")
  }
  rval
}

fast_glm.wfit <- function(y, X, intercept = TRUE, weights = NULL,
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
  ll.new <- ll.fast_glm(fam, aic.model, rank)
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
    "coefficients" = coefficients, "logLik" = ll.new, "iter" = iter,
    "tol" = tol, "family" = family, "link" = link, "df" = dfr, "XTX" = XTX,
    "dispersion" = dispersion, "ok" = ok, "rank" = rank, "RSS" = RSS, method = method,
    "aic" = aic.model, "deviance" = dev, "nulldf" = nulldf,
    "nulldev" = nulldev, "ngoodobs" = n.ok, "n" = nobs, "intercept" = intercept,
    "convergence" = (!(tol > tol.estimation))
  )
  class(rval) <- "fast_glm"
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
  ll.new <- ll.fast_glm(fam, aic.model, rank)
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
  class(rval) <- "fast_glm"
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

ll.fast_glm <- function(family, aic.model, nvar) {
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
