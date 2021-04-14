# Dynamically exported, see zzz.R

matpower <- function(X, p) {
  if ((ncol(X) == 1L) && (nrow(X) == 1L)) {
    return(X^p)
  }
  Xeig <- eigen(X, symmetric = TRUE)
  if (any(Xeig$values < 0)) {
    stop("matrix is not positive semidefinite")
  }
  sqomega <- diag(Xeig$values^p)
  return(Xeig$vectors %*% sqomega %*% t(Xeig$vectors))
}
