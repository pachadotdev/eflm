#' Fitting Generalized Linear Models
#'
#' fglm is used to fit generalized linear models, specified by giving a symbolic
#' description of the linear predictor and a description of the error
#' distribution.
#'
#' @param formula A formula for the model
#' @param data A tibble or data.frame
#' @param family See the function \link{glm}, but here it must be specified with brackets (e.g. \code{quasipoisson()})
#' @param intercept Logical value to determine whearead to included an intercept in the null model (Defaults to \code{TRUE})
#' @param weights An optional vector of ‘prior weights’ to be used in the fitting process. Should be \code{NULL}
#' or a numeric vector (e.g. \code{data$weights}, defaults to \code{NULL})
#' @param na.action a function which indicates what should happen when the data
#' contain NAs. The default is set by the \code{na.action} setting of options,
#' and is \link{na.fail} if that is unset. The ‘factory-fresh’ default is
#' \code{na.omit}. Another possible value is NULL, no action. Value
#' \code{na.exclude} can be useful.
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
#' @param singularity.method The chosen method to detect for singularity. Defaults to \code{"eigen"} but
#' it can also be \code{"Cholesky"} or \code{"qr"}
#' @param x Logical value indicating whether the model matrix used in the fitting process
#' should be returned as components of the returned value (Defaults to \code{FALSE}, see the function \link{glm.fit})
#' @param y Logical value indicating whether the response vector used in the fitting process
#' should be returned as components of the returned value (Defaults to \code{FALSE}, see the function \link{glm.fit})
#' @param tol.estimation Tolerance to be used for the estimation (Defaults to 1e-8)
#' @param tol.solve (Defaults to \code{.Machine$double.eps}, see the function \link{solve})
#' @param tol.values Tolerance to consider eigenvalues equal to zero (Defaults to 1e-7, see the function \link{control}),
#' @param tol.vectors Tolerance to consider eigenvectors equal to zero (Defaults to 1e-7, see the function \link{control})
#' @param \dots For glm: arguments to be used to form the default control argument if it is not supplied directly. For weights: further arguments passed to or from other methods.
#' @importFrom stats gaussian na.pass
#' @export

fglm <- function(formula, data, family = gaussian(), weights = NULL,
                 na.action = na.omit, start = NULL, etastart = NULL,
                 mustart = NULL, offset = NULL, maxit = 25, k = 2, model = TRUE,
                 singularity.method = c("eigen", "Cholesky", "qr"),
                 intercept = TRUE, x = FALSE, y = TRUE,
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
  singularity.method <- match.arg(singularity.method)
  rval <- fglm.wfit(
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
    singularity.method = singularity.method
  )
  rval$terms <- tf
  rval$call <- call
  class(rval) <- c("fglm", "flm")
  if (model) rval$model <- M
  rval$fitted.values <- predict.fglm(rval, newdata = M, type = "response", na.action = na.action)
  rval$linear.predictors <- predict.fglm(rval, newdata = M, type = "link", na.action = na.action)
  if (x) rval$x <- X
  if (target) {
    rval$y <- y
    names(rval$y) <- names(rval$fitted.values)
  }
  names(rval$prior.weights) <- names(rval$fitted.values)

  if ((rval$iter == maxit) & (!rval$convergence)) {
    warning("Maximum number of iterations reached without convergence")
  }
  rval
}

fglm.wfit <- function(y, X, intercept = TRUE, weights = NULL,
                      family = gaussian(), start = NULL, etastart = NULL,
                      mustart = NULL, offset = NULL, maxit = 25, k = 2,
                      tol.estimation = 1e-8, tol.values = 1e-7,
                      tol.vectors = 1e-7, tol.solve = .Machine$double.eps,
                      singularity.method = c('eigen','Cholesky','qr'), ...) {
  nobs <- NROW(y)
  nvar <- ncol(X)
  if (missing(y)) stop("Argument y is missing")
  if (missing(X)) stop("Argument X is missing")
  if (is.null(offset)) offset <- rep.int(0, nobs)
  if (is.null(weights)) weights <- rep(1, nobs)
  col.names <- dimnames(X)[[2]]
  singularity.method <- match.arg(singularity.method)
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
    if (iter == 1 & singularity.method != "qr") {
      variable <- colnames(X)
      ris <- control(XTX, , tol.values, tol.vectors, , singularity.method)
      ok <- ris$pivot[1:ris$rank]
      XTX <- ris$XTX
      X <- X[, ok]
      XTz <- XTz[ok]
      start <- start[ok]
      beta <- start
    }
    if (singularity.method == "qr") {
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
  if (singularity.method == "qr") {
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
    "singularity.method" = singularity.method,
    "aic" = aic.model,
    "deviance" = dev,
    "df.null" = nulldf,
    "null.deviance" = nulldev,
    "weights" = W,
    "prior.weights" = weights,
    "n.good.obs" = n.ok,
    "n" = nobs,
    "intercept" = intercept,
    "convergence" = (!(tol > tol.estimation))
  )
  class(rval) <- "fglm"
  return(rval)
}

coef.fglm <- function(object, ...) { object$coefficients }

vcov.fglm <- function(object, ...) { object$dispersion * solve(object$XTX) }

deviance.fglm <- function(object, ...) { object$deviance }

nobs.fglm <- function(object, use.fallback = FALSE, ...) {
  if (!is.null(w <- object$weights)) sum(w != 0) else object$n
}

#' @importFrom stats residuals weights is.ts ts start frequency
#' @importFrom zoo is.zoo zoo index
estfun.glm <- function(x, ...) {
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
