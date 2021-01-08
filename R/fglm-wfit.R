fglm.wfit <- function(y, X, intercept = TRUE, weights = NULL,
                      family = gaussian(), start = NULL, etastart = NULL,
                      mustart = NULL, offset = NULL, maxit = 25, k = 2,
                      tol.estimation = 1e-8, tol.values = 1e-7,
                      tol.vectors = 1e-7, tol.solve = .Machine$double.eps,
                      method = c('eigen','Cholesky','qr'), ...) {
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
    # names(W) <- ynames
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
    "method" = method,
    "aic" = aic.model,
    "deviance" = dev,
    "df.null" = nulldf,
    "null.deviance" = nulldev,
    "weights" = W,
    "ngoodobs" = n.ok,
    "n" = nobs,
    "intercept" = intercept,
    "convergence" = (!(tol > tol.estimation))
  )
  class(rval) <- "fglm"
  return(rval)
}
