# the same as stats:::safe_pchisq
#' @importFrom stats pchisq
safe_pchisq <- function(q, df, ...) {
  df[df <= 0] <- NA
  pchisq(q = q, df = df, ...)
}
