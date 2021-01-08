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

coef.fglm <- function(object, ...) object$coefficients

vcov.fglm <- function(object, ...) object$dispersion * solve(object$XTX)

deviance.fglm <- function(object, ...) object$deviance

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
