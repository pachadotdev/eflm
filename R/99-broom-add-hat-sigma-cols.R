# Dynamically exported, see zzz.R

# taken from broom::: but using base when possible
add_hat_sigma_cols <- function (df, x, infl)
{
  df$.hat <- 0
  df$.sigma <- 0
  w <- x$weights
  nonzero_idx <- if (is.null(w))
    seq_along(df$.hat)
  else which(w != 0)
  df$.hat[nonzero_idx] <- unname(infl$hat)
  df$.sigma[nonzero_idx] <- unname(infl$sigma)
  df
}
