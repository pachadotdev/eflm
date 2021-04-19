# Dynamically exported, see zzz.R

# taken from broom::: (makes augment.elm work) but using base when possible
#' @importFrom stats terms
augment_newdata <- function(x, data, newdata, .se_fit, interval = NULL, ...) {
  passed_newdata <- !is.null(newdata)
  df <- if (passed_newdata) newdata else data
  df <- as_augment_tibble(df)
  response_var_in_newdata <- is.element(all.vars(x$call)[[1]], names(df))
  if (.se_fit) {
    pred_obj <- predict(x, newdata = newdata, na.action = na.pass, se.fit = .se_fit, interval = interval, ...)
    if (is.null(interval) || interval == "none") {
      df$.fitted <- unname(pred_obj$fit)
    } else {
      df$.fitted <- pred_obj$fit[, "fit"]
      df$.lower <- pred_obj$fit[, "lwr"]
      df$.upper <- pred_obj$fit[, "upr"]
    }
    se_idx <- which(names(pred_obj) %in% c("se.fit", "se"))
    df$.se.fit <- pred_obj[[se_idx]]
  } else if (!is.null(interval) && interval != "none") {
    pred_obj <- predict(x, newdata = newdata, na.action = na.pass, se.fit = FALSE, interval = interval, ...)
    df$.fitted <- pred_obj[, "fit"]
    df$.lower <- pred_obj[, "lwr"]
    df$.upper <- pred_obj[, "upr"]
  } else if (passed_newdata) {
    if (is.null(interval) || interval == "none") {
      df$.fitted <- unname(predict(x, newdata = newdata, na.action = na.pass, ...))
    } else {
      pred_obj <- predict(x, newdata = newdata, na.action = na.pass, interval = interval, ...)
      df$.fitted <- pred_obj$fit[, "fit"]
      df$.lower <- pred_obj$fit[, "lwr"]
      df$.upper <- pred_obj$fit[, "upr"]
    }
  } else {
    if (is.null(interval) || interval == "none") {
      df$.fitted <- unname(predict(x, na.action = na.pass, ...))
    } else {
      pred_obj <- predict(x, newdata = newdata, na.action = na.pass, interval = interval, ...)
      df$.fitted <- pred_obj$fit[, "fit"]
      df$.lower <- pred_obj$fit[, "lwr"]
      df$.upper <- pred_obj$fit[, "upr"]
    }
  }

  response <- function(object, newdata = NULL) {
    model.response(model.frame(terms(object), data = newdata, na.action = na.pass))
  }
  safe_response <- purrr::possibly(response, NULL)

  resp <- safe_response(x, df)
  if (!is.null(resp) && is.numeric(resp)) {
    df$.resid <- unname((resp - df$.fitted))
  }
  df
}
