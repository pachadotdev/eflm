# Dynamically exported, see zzz.R
AIC.fglm <- function(object, ...) {
  if (!(length(list(...)))) {
    object$aic
  } else {
    aic <- function(x) x$aic
    object <- list(object, ...)
    val <- sapply(object, aic)
    val
  }
}
