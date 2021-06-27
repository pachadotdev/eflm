#' fast generalized linear model fitting
#'
#' @param formula an object of class \code{"\link{formula}"} (or one that can be
#'  coerced to that class): a symbolic description of the model to be fitted.
#'  The details of model specification are given under \sQuote{Details}.
#' @param family a description of the error distribution and link function to be
#'  used in the model. This can be a character string naming a
#'  family function, a family function or the result of a call to a family
#'  function. See \code{\link{family}} for details of family functions.
#' @param data an optional data frame, list or environment (or object coercible
#'  by \code{\link{as.data.frame}} to a data frame) containing the variables in
#'  the model. If not found in \code{data}, the variables are taken from
#'  \code{environment(formula)}, typically the environment from which \code{lm}
#'  is called.
#' @param weights an optional vector of weights to be used in the fitting
#'  process. Should be \code{NULL} or a numeric vector. If non-NULL, weighted
#'  least squares is used with weights \code{weights} (that is, minimizing
#'  \code{sum(w*e^2)}); otherwise ordinary least squares is used.
#' @param start starting values for the parameters in the linear predictor.
#' @param etastart starting values for the linear predictor.
#' @param mustart starting values for the vector of means.
#' @param offset this can be used to specify an \emph{a priori} known component
#'  to be included in the linear predictor during fitting. This should be
#'  \code{NULL} or a numeric vector or matrix of extents matching those of the
#'  response. One or more \code{\link{offset}} terms can be included in the
#'  formula instead or as well, and if more than one are specified their sum is
#'  used. See \code{\link{model.offset}}.
#' @param method a string with value \code{"qr"} for the column-pivoted QR
#'  decomposition (exactly as base R) or \code{"chol"} for the LDLT Cholesky
#'  decomposition (slower than LLT Cholesky but more stable)
#' @param tol threshold tolerance for convergence. Should be a positive real number.
#' @param maxit maximum number of IRLS iterations. Should be an integer.
#' @return A list with the elements
#' \item{coefficients}{a vector of coefficients}
#' \item{se}{a vector of the standard errors of the coefficient estimates}
#' \item{rank}{a scalar denoting the computed rank of the model matrix}
#' \item{df.residual}{a scalar denoting the degrees of freedom in the model}
#' \item{residuals}{the vector of residuals}
#' \item{s}{a numeric scalar - the root mean square for residuals}
#' \item{fitted.values}{the vector of fitted values}
#' @export
eglm <- function(model, ...) UseMethod("eglm")


#' @method eglm default
#' @export
eglm.default <- function(model,
                            family = gaussian(),
                            data,
                            weights = NULL,
                            start = NULL,
                            etastart = NULL,
                            mustart = NULL,
                            offset = NULL,
                            method = "qr",
                            tol = 1e-8,
                            maxit = 100L,
                            ...) {
  ## model
  if (is.character(model)) {
    model <- as.formula(model)
  }
  if (is.null(model)) {
    print(model)
    stop("'model' not recognized")
  }

  ## y,x
  if (is.data.frame(data)) {
    y <- unlist(data[, as.character(model[[2]])])
    x <- model.matrix(model, data)

    stopifnot(is.matrix(x), is.numeric(y), NROW(y) == nrow(x))
  } else {
    stop("'data' should be a data.frame or tibble")
  }

  ## family
  if (is.character(family)) {
    family <- get(family, mode = "function", envir = parent.frame())
  }
  if (is.function(family)) family <- family()
  if (is.null(family$family)) {
    print(family)
    stop("'family' not recognized")
  }

  # y             <- as.numeric(y)

  ## avoid problems with 1D arrays, but keep names
  if (length(dim(y)) == 1L) {
    nm <- rownames(y)
    dim(y) <- NULL
    if (!is.null(nm)) names(y) <- nm
  }

  nobs <- NROW(y)

  aic <- family$aic

  if (is.null(weights)) weights <- rep(1, nobs)
  if (is.null(offset)) offset <- rep(0, nobs)

  stopifnot(NROW(y) == NROW(weights), NROW(y) == NROW(offset))

  res <- eglm.wfit(
    x, y, family, weights, offset,
    start, etastart, mustart,
    method, tol, maxit
  )
  y <- res$y

  res$residuals <- (y - res$fitted.values) / family$mu.eta(res$linear.predictors)

  # from summary.glm()
  dispersion <-
    if (family$family %in% c("poisson", "binomial")) {
      1
    } else if (res$df.residual > 0) {
      est.disp <- TRUE
      if (any(weights == 0)) {
        warning("observations with zero weight not used for calculating dispersion")
      }
      sum((res$weights * res$residuals^2)[weights > 0]) / res$df.residual
    } else {
      est.disp <- TRUE
      NaN
    }

  res$dispersion <- dispersion

  if (!is.nan(dispersion)) res$se <- res$se * sqrt(dispersion)

  wtdmu <- if (res$intercept) sum(weights * y) / sum(weights) else family$linkinv(offset)
  nulldev <- sum(family$dev.resids(y, wtdmu, weights))

  n.ok <- nobs - sum(weights == 0)
  nulldf <- n.ok - as.integer(res$intercept)
  res$df.null <- nulldf

  res$null.deviance <- nulldev

  rank <- res$rank
  dev <- res$deviance

  aic.model <- aic(y, res$n, res$fitted.values, res$prior.weights, dev) + 2 * rank

  res$aic <- aic.model

  # will change later
  boundary <- FALSE

  if (boundary) {
    warning("fit_glm: algorithm stopped at boundary value", call. = FALSE)
  }

  res$call <- match.call()

  class(res) <- "eglm"
  res
}


eglm.wfit <- function(x, y,
                         family = gaussian(),
                         weights = rep(1, NROW(y)),
                         offset = rep(0, NROW(y)),
                         start = NULL,
                         etastart = NULL,
                         mustart = NULL,
                         method = "qr",
                         tol = 1e-7,
                         maxit = 100L) {
  weights <- as.vector(weights)
  offset <- as.vector(offset)
  stopifnot(
    is.numeric(weights),
    is.numeric(offset),
    is.numeric(tol),
    is.numeric(maxit),
    tol[1] > 0,
    maxit[1] > 0
  )

  nobs <- n <- NROW(y)
  nvars <- NCOL(x)
  if (is.null(family$family)) {
    print(family)
    stop("'family' not recognized")
  }

  if (any(weights < 0)) stop("negative weights not allowed")

  if (!any(method[1] %in% c("qr", "chol"))) {
    stop("Invalid decomposition method specified. Choose 'qr' or 'chol'.")
  }

  cnames <- colnames(x)

  # from glm
  variance <- family$variance
  dev.resids <- family$dev.resids
  aic <- family$aic
  linkinv <- family$linkinv
  mu.eta <- family$mu.eta

  unless.null <- function(x, if.null) if (is.null(x)) if.null else x
  valideta <- unless.null(family$valideta, function(eta) TRUE)
  validmu <- unless.null(family$validmu, function(mu) TRUE)


  if (is.null(mustart)) {
    ## calculates mustart and may change y and weights and set n (!)
    eval(family$initialize)
  } else {
    mukeep <- mustart
    eval(family$initialize)
    mustart <- mukeep
  }

  y <- as.numeric(y)

  coefold <- NULL
  eta <-
    if (!is.null(etastart)) {
      etastart
    } else if (!is.null(start)) {
      if (length(start) != nvars) {
        stop(gettextf("length of 'start' should equal %d", nvars),
          domain = NA
        )
      } else {
        coefold <- start
        offset + as.vector(if (NCOL(x) == 1L) x * start else x %*% start)
      }
    } else {
      family$linkfun(mustart)
    }
  mu <- linkinv(eta)

  if (!(validmu(mu) && valideta(eta))) {
    stop("cannot find valid starting values: please specify some", call. = FALSE)
  }

  if (is.null(start)) start <- rep(0, nvars)

  method <- switch(method,
    "qr" = 1,
    "chol" = 2
  )

  res <- fit_glm(
    x, drop(y), drop(weights), drop(offset),
    drop(start), drop(mu), drop(eta),
    family$variance, family$mu.eta, family$linkinv, family$dev.resids,
    family$valideta, family$validmu,
    method[1], as.double(tol[1]), as.integer(maxit[1])
  )

  res$intercept <- any(is.int <- colMax_dense(x) == colMin_dense(x))

  if (!res$converged) {
    warning("fit_glm: algorithm did not converge", call. = FALSE)
  }

  eps <- 10 * .Machine$double.eps
  if (family$family == "binomial") {
    if (any(res$fitted.values > 1 - eps) || any(res$fitted.values < eps)) {
      warning("fit_glm: fitted probabilities numerically 0 or 1 occurred", call. = FALSE)
    }
  }
  if (family$family == "poisson") {
    if (any(res$fitted.values < eps)) {
      warning("fit_glm: fitted rates numerically 0 occurred", call. = FALSE)
    }
  }

  if (is.null(cnames)) {
    ncx <- ncol(x)
    if (res$intercept) {
      which.int <- which(is.int)
      cnames <- paste0("X", 1:(ncx - 1))
      names(res$coefficients) <- 1:ncx
      names(res$coefficients)[-which.int] <- cnames
      names(res$coefficients)[which.int] <- "(Intercept)"
    } else {
      names(res$coefficients) <- paste0("X", 1:ncx)
    }
  } else {
    names(res$coefficients) <- cnames
  }

  res$family <- family
  res$prior.weights <- weights
  res$y <- y
  res$n <- n
  res
}
