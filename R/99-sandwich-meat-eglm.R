# Dynamically exported, see zzz.R

meat.eglm <- function(x, adjust = FALSE, ...) {
  if (is.list(x) && !is.null(x$na.action)) {
    class(x$na.action) <- "omit"
  }
  psi <- sandwich::estfun(x)
  k <- NCOL(psi)
  n <- NROW(psi)
  rval <- crossprod(as.matrix(psi)) / n
  if (adjust) {
    rval <- n / (n - k) * rval
  }
  rownames(rval) <- colnames(rval) <- colnames(psi)
  return(rval)
}
