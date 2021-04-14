# Dynamically exported, see zzz.R

vcovBS.eglm <- function(x, cluster = NULL, R = 250, start = FALSE, ..., fix = FALSE, use = "pairwise.complete.obs", applyfun = NULL, cores = NULL) {
  ## set up return value with correct dimension and names
  cf <- coef(x)
  if (identical(start, TRUE)) start <- cf
  if (identical(start, FALSE)) start <- NULL
  k <- length(cf)
  n <- nobs(x)
  rval <- matrix(0, nrow = k, ncol = k, dimnames = list(names(cf), names(cf)))
  cf <- matrix(rep.int(NA_real_, k * R), ncol = k, dimnames = list(NULL, names(cf)))

  ## cluster can either be supplied explicitly or
  ## be an attribute of the model...FIXME: other specifications?
  if (is.null(cluster)) cluster <- attr(x, "cluster")

  ## resort to cross-section if no clusters are supplied
  if (is.null(cluster)) cluster <- 1L:n

  ## collect 'cluster' variables in a data frame
  if (inherits(cluster, "formula")) {
    cluster_tmp <- if ("Formula" %in% loadedNamespaces()) { ## FIXME to suppress potential warnings due to | in Formula
      suppressWarnings(expand.model.frame(x, cluster, na.expand = FALSE))
    } else {
      expand.model.frame(x, cluster, na.expand = FALSE)
    }
    cluster <- model.frame(cluster, cluster_tmp, na.action = na.pass)
  } else {
    cluster <- as.data.frame(cluster)
  }

  ## handle omitted or excluded observations
  if ((n != NROW(cluster)) && !is.null(x$na.action) && (class(x$na.action) %in% c("exclude", "omit"))) {
    cluster <- cluster[-x$na.action, , drop = FALSE]
  }

  if (NROW(cluster) != n) stop("number of observations in 'cluster' and 'nobs()' do not match")

  ## for multi-way clustering: set up interaction patterns
  p <- NCOL(cluster)
  if (p > 1L) {
    cl <- lapply(1L:p, function(i) combn(1L:p, i, simplify = FALSE))
    cl <- unlist(cl, recursive = FALSE)
    sign <- sapply(cl, function(i) (-1L)^(length(i) + 1L))
    paste_ <- function(...) paste(..., sep = "_")
    for (i in (p + 1L):length(cl)) {
      cluster <- cbind(cluster, Reduce(paste_, unclass(cluster[, cl[[i]]]))) ## faster than: interaction()
    }
  } else {
    cl <- list(1)
    sign <- 1
  }

  ## model information: original response and design matrix or the corresponding fitted/residuals/QR
  y <- if (!is.null(x$y)) {
    x$y
  } else if (!is.null(x$model)) {
    model.response(x$model)
  } else {
    model.response(model.frame(x))
  }
  xfit <- model.matrix(x)

  ## apply infrastructure for refitting models
  if (is.null(applyfun)) {
    applyfun <- if (is.null(cores)) {
      lapply
    } else {
      if (.Platform$OS.type == "windows") {
        cl_cores <- parallel::makeCluster(cores)
        on.exit(parallel::stopCluster(cl_cores))
        function(X, FUN, ...) parallel::parLapply(cl = cl_cores, X, FUN, ...)
      } else {
        function(X, FUN, ...) parallel::mclapply(X, FUN, ..., mc.cores = cores)
      }
    }
  }

  ## bootstrap for each cluster dimension
  for (i in 1L:length(cl))
  {
    ## cluster structure
    cli <- split(seq_along(cluster[[i]]), cluster[[i]])

    ## bootstrap fitting function
    bootfit <- function(j, ...) {
      j <- unlist(cli[sample(names(cli), length(cli), replace = TRUE)])
      eglm.wfit(
        y = y[j],
        X = xfit[j, , drop = FALSE],
        family = x$family,
        start = start, ...
      )$coefficients
    }

    ## actually refit
    cf <- applyfun(1L:R, bootfit, ...)
    cf <- do.call("rbind", cf)

    ## aggregate across cluster variables
    rval <- rval + sign[i] * stats::cov(cf, use = use)
  }

  ## check (and fix) if sandwich is not positive semi-definite
  if (fix && any((eig <- eigen(rval, symmetric = TRUE))$values < 0)) {
    eig$values <- pmax(eig$values, 0)
    rval[] <- crossprod(sqrt(eig$values) * t(eig$vectors))
  }
  return(rval)
}
