#' @importFrom stats setNames vcov qt
confint.elm <- function(object, parm, level = 0.95, ...) {
  cf <- coef(object)
  ses <- sqrt(diag(vcov(object))) # gives NA for aliased parms
  pnames <- names(ses) # ok for "mlm", too
  if (is.matrix(cf)) cf <- setNames(as.vector(cf), pnames) # for "mlm"
  if (missing(parm)) {
    parm <- pnames
  } else if (is.numeric(parm)) parm <- pnames[parm]
  ## else 'parm' must contain parameter names matching those in 'pnames'
  a <- (1 - level) / 2
  a <- c(a, 1 - a)
  fac <- qt(a, object$df.residual) # difference from default method
  pct <- format.perc(a, 3)
  ci <- array(NA_real_,
    dim = c(length(parm), 2L), dimnames = list(parm, pct)
  )
  ci[] <- cf[parm] + ses[parm] %o% fac
  ci
}
