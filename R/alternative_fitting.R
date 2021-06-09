control <- function(B, symmetric = TRUE, tol.values = 1e-07, tol.vectors = 1e-07,
                    out.B = TRUE, method = "chol") {
  method <- match.arg(method)
  if (method != "chol") {
    stop("method not valid or not implemented")
  }
  A <- chol(B, pivot = TRUE)
  pivot <- attributes(A)$"pivot"
  rank <- attributes(A)$"rank"
  ok <- sort(pivot[1:rank])
  nok <- if (rank < length(pivot)) pivot[(rank + 1):length(pivot)] else NULL
  B <- B[ok, ok]
  rval <- if (out.B) {
    list(XTX = B, rank = rank, pivot = c(ok, nok))
  } else {
    list(rank = rank, pivot = c(ok, nok))
  }

  rval
}
