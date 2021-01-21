control <- function(B, symmetric = TRUE, tol.values = 1e-07, tol.vectors = 1e-07,
                    out.B = TRUE, singularity.method = c("eigen", "Cholesky")) {
  singularity.method <- match.arg(singularity.method)
  if (!(singularity.method %in% c("eigen", "Cholesky"))) {
    stop("singularity.method not valid or not implemented")
  }
  if (singularity.method == "eigen") {
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
  if (singularity.method == "Cholesky") {
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
