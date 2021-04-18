# Dynamically exported, see zzz.R

# taken from broom::: but using base when possible
augment.elm <- function(x, data = model.frame(x), newdata = NULL,
                        se_fit = FALSE, interval = c("none", "confidence", "prediction"), ...) {
  interval <- match.arg(interval)
  df <- augment_newdata(x, data, newdata, se_fit, interval)
  if (is.null(newdata)) {
    tryCatch(
      {
        infl <- influence(x, do.coef = FALSE)
        df <- add_hat_sigma_cols(df, x, infl)
      },
      error = data_error
    )
  }
  df
}
