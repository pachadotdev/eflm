# the same as stats:::safe_pchisq
#' @importFrom stats pchisq
safe_pchisq <- function(q, df, ...) {
  df[df <= 0] <- NA
  pchisq(q = q, df = df, ...)
}

# the same as stats:::safe_pchisq
#' @importFrom stats pf
safe_pf <- function(q, df1, ...) {
  df1[df1 <= 0] <- NA
  pf(q = q, df1 = df1, ...)
}

Fstat <- function(table, rdf) {
  dev <- table$Deviance
  df <- table$Df
  diff <- pmax(0, (dev[1L] - dev) / df)
  Fs <- diff / (dev / (rdf - df))
  Fs[df < .Machine$double.eps] <- NA
  P <- Fs
  nnas <- !is.na(Fs)
  P[nnas] <- safe_pf(Fs[nnas], df[nnas], rdf - df[nnas],
    lower.tail = FALSE
  )
  list(Fs = Fs, P = P)
}
