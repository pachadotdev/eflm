# Dynamically exported, see zzz.R

# taken from broom::: (makes tidy.fglm work)
#' @importFrom stats confint
confint_terms <- function (x, ...) {
  ci <- suppressMessages(confint(x, ...))
  if (is.null(dim(ci))) {
    ci <- matrix(ci, nrow = 1)
    rownames(ci) <- names(coef(x))[1]
  }
  ci <- tibble::as_tibble(ci, rownames = "term")
  names(ci) <- c("term", "conf.low", "conf.high")
  ci
}

# taken from broom::: but adapted to use base (makes tidy.fglm work)
exponentiate <- function (data) {
  data[, "estimate"] <- exp(data[, "estimate"])
  if ("conf.low" %in% colnames(data)) {
    data[, "conf.low"] <- exp(data[, "conf.low"])
    data[, "conf.high"] <- exp(data[, "conf.high"])
  }
  return(data)
}

# taken from broom::: but adapted to use (mostly) base
tidy.fglm <- function (x, conf.int = FALSE, conf.level = 0.95, exponentiate = FALSE, ...) {
  ret <- tibble::as_tibble(summary(x)$coefficients, rownames = "term")
  colnames(ret) <- c("term", "estimate", "std.error", "statistic", "p.value")
  if (is.character(ret$p.value)) ret$p.value <- as.numeric(ret$p.value)
  if (conf.int) {
    ci <- confint_terms(x, level = conf.level)
    ret <- merge(ret, ci, by = "term")
  }
  if (exponentiate) {
    ret <- exponentiate(ret)
  }
  ret
}

# taken from broom::: to make augment.fglm work
as_augment_tibble <- function (data)
{
  if (inherits(data, "matrix") & is.null(colnames(data))) {
    stop("The supplied `data`/`newdata` argument was an unnamed matrix. ",
         "Please supply a matrix or dataframe with column names.")
  }
  tryCatch(df <- tibble::as_tibble(data), error = function(cnd) {
    stop("Could not coerce data to `tibble`. Try explicitly passing a",
         "dataset to either the `data` or `newdata` argument.",
         call. = FALSE)
  })
  if (has_rownames(data)) {
    df <- tibble::add_column(df, .rownames = rownames(data),
                             .before = TRUE)
  }
  df
}

# taken from broom::: to make augment.fglm work
has_rownames <- function (df) {
  if (tibble::is_tibble(df)) {
    return(FALSE)
  }
  any(rownames(df) != as.character(1:nrow(df)))
}

# taken from brooom::: to make augment.fglm work
data_error <- function (cnd) {
  stop("Must specify either `data` or `newdata` argument.",
       call. = FALSE)
}

# taken from broom::: but using base when possible
#' @importFrom stats rstandard
augment.fglm <- function (x, data = NULL, newdata = NULL,
                          type.predict = c("link","response", "terms"),
                          type.residuals = c("deviance", "pearson"),
                          se_fit = FALSE, ...) {
  type.predict <- match.arg(type.predict)
  type.residuals <- match.arg(type.residuals)
  if (is.null(data)) data <- try(eval(x$call$data))
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
    tryCatch({
      infl <- stats::influence(x, do.coef = FALSE)
      df$.resid <- unname(residuals(x, type = type.residuals))
      df$.std.resid <- unname(rstandard(x, infl = infl, type = type.residuals))
      df <- add_hat_sigma_cols(df, x, infl)
      df$.cooksd <- unname(cooks.distance(x, infl = infl))
    }, error = data_error)
  }
  df
}
