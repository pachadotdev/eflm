# the same as stats:::safe_pchisq
#' @importFrom stats pf
safe_pf <- function(q, df1, ...) {
  df1[df1 <= 0] <- NA
  pf(q = q, df1 = df1, ...)
}
