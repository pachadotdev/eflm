qr.lm <- function(x, ...) {
  if (is.null(r <- x$qr)) {
    stop("elm object does not have a proper 'qr' component.
 Rank zero or should not have used elm(.., qr=FALSE).")
  }
  r
}
