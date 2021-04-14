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
