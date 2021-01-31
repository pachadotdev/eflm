# Dynamically exported, see zzz.R

#' @importFrom stats expand.model.frame hatvalues
#' @importFrom utils combn
meatCL.bglm <- function (x, cluster = NULL, type = NULL, cadjust = TRUE, multi0 = FALSE,
                         ...) {
  if (is.list(x) && !is.null(x$na.action))
    class(x$na.action) <- "omit"
  ef <- sandwich::estfun(x, ...)
  k <- NCOL(ef)
  n <- NROW(ef)
  rval <- matrix(0, nrow = k, ncol = k, dimnames = list(colnames(ef),
                                                        colnames(ef)))
  if (is.null(cluster))
    cluster <- attr(x, "cluster")
  if (is.null(cluster))
    cluster <- 1L:n
  if (inherits(cluster, "formula")) {
    cluster_tmp <- if ("Formula" %in% loadedNamespaces()) {
      suppressWarnings(expand.model.frame(x, cluster,
                                          na.expand = FALSE))
    }
    else {
      expand.model.frame(x, cluster, na.expand = FALSE)
    }
    cluster <- model.frame(cluster, cluster_tmp, na.action = na.pass)
  }
  else {
    cluster <- as.data.frame(cluster)
  }
  if ((n != NROW(cluster)) && !is.null(x$na.action) && (class(x$na.action) %in%
                                                        c("exclude", "omit"))) {
    cluster <- cluster[-x$na.action, , drop = FALSE]
  }
  if (NROW(cluster) != n)
    stop("number of observations in 'cluster' and 'estfun()' do not match")
  p <- NCOL(cluster)
  if (p > 1L) {
    cl <- lapply(1L:p, function(i) combn(1L:p, i, simplify = FALSE))
    cl <- unlist(cl, recursive = FALSE)
    sign <- sapply(cl, function(i) (-1L)^(length(i) + 1L))
    paste_ <- function(...) paste(..., sep = "_")
    for (i in (p + 1L):length(cl)) {
      cluster <- cbind(cluster, Reduce(paste_, unclass(cluster[,
                                                               cl[[i]]])))
    }
    if (multi0)
      cluster[[length(cl)]] <- 1L:n
  }
  else {
    cl <- list(1)
    sign <- 1
  }
  g <- sapply(1L:length(cl), function(i) {
    if (is.factor(cluster[[i]])) {
      length(levels(cluster[[i]]))
    }
    else {
      length(unique(cluster[[i]]))
    }
  })
  if (is.null(type)) {
    type <- if (class(x)[1L] == "lm")
      "HC1"
    else "HC0"
  }
  type <- match.arg(type, c("HC", "HC0", "HC1", "HC2", "HC3"))
  if (type == "HC")
    type <- "HC0"
  if (type %in% c("HC2", "HC3")) {
    if (any(g == n))
      h <- hatvalues(x)
    if (!all(g == n)) {
      if (!(class(x)[1L] %in% "bglm"))
        warning("clustered HC2/HC3 are only applicable to (fast generalized) linear regression models")
      X <- model.matrix(x)
      if (any(alias <- is.na(coef(x))))
        X <- X[, !alias, drop = FALSE]
      attr(X, "assign") <- NULL
      w <- weights(x, "working")
      XX1 <- if (is.null(w))
        chol2inv(qr.R(qr(X)))
      else chol2inv(qr.R(qr(X * sqrt(w))))
      res <- rowMeans(ef/X, na.rm = TRUE)
      res[apply(abs(ef) < .Machine$double.eps, 1L, all)] <- 0
    }
  }
  for (i in 1L:length(cl)) {
    efi <- ef
    adj <- if (multi0 & (i == length(cl))) {
      if (type == "HC1")
        (n - k)/(n - 1L)
      else 1
    }
    else {
      if (cadjust)
        g[i]/(g[i] - 1L)
      else 1
    }
    if (type %in% c("HC2", "HC3")) {
      if (g[i] == n) {
        efi <- if (type == "HC2") {
          efi/sqrt(1 - h)
        }
        else {
          efi/(1 - hatvalues(x))
        }
      }
      else {
        for (j in unique(cluster[[i]])) {
          ij <- which(cluster[[i]] == j)
          Hij <- if (is.null(w)) {
            X[ij, , drop = FALSE] %*% XX1 %*% t(X[ij,
                                                  , drop = FALSE])
          }
          else {
            X[ij, , drop = FALSE] %*% XX1 %*% t(X[ij,
                                                  , drop = FALSE]) %*% diag(w[ij], nrow = length(ij),
                                                                            ncol = length(ij))
          }
          Hij <- if (type == "HC2") {
            matpower(diag(length(ij)) - Hij, -0.5)
          }
          else {
            solve(diag(length(ij)) - Hij)
          }
          efi[ij, ] <- drop(Hij %*% res[ij]) * X[ij,
                                                 , drop = FALSE]
        }
      }
      efi <- sqrt((g[i] - 1L)/g[i]) * efi
    }
    efi <- if (g[i] < n)
      apply(efi, 2L, rowsum, cluster[[i]])
    else efi
    rval <- rval + sign[i] * adj * crossprod(efi)/n
  }
  if (type == "HC1")
    rval <- (n - 1L)/(n - k) * rval
  return(rval)
}
