# Dynamically exported, see zzz.R

# taken from broom::: but using base when possible
augment.eglm <- function(x, data = NULL, newdata = NULL,
                         type.predict = c("link", "response", "terms"),
                         type.residuals = c("deviance", "pearson"),
                         se_fit = FALSE, ...) {
  type.predict <- match.arg(type.predict)
  type.residuals <- match.arg(type.residuals)
  if (is.null(newdata)) data <- try(eval(x$call$data))
  if (any(class(data) %in% "try-error")) data <- NULL
  df <- if (is.null(newdata)) data else newdata
  df <- as_augment_tibble(df)
  if (se_fit) {
    pred_obj <- predict(x, newdata, type = type.predict, se.fit = TRUE)
    df$.fitted <- unname(pred_obj$fit)
    df$.se.fit <- unname(pred_obj$se.fit)
  } else {
    df$.fitted <- unname(predict(x, newdata, type = type.predict))
  }
  if (is.null(newdata)) {
    tryCatch(
      {
        infl <- stats::influence(x, do.coef = FALSE)
        df$.resid <- unname(residuals(x, type = type.residuals))
        df$.std.resid <- unname(rstandard(x, infl = infl, type = type.residuals))
        df <- add_hat_sigma_cols(df, x, infl)
        df$.cooksd <- unname(cooks.distance(x, infl = infl))
      },
      error = data_error
    )
  }
  df
}
