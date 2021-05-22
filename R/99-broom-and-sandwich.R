# taken from vctrs
s3_register <- function (generic, class, method = NULL) {
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)
  pieces <- strsplit(generic, "::")[[1]]
  stopifnot(length(pieces) == 2)
  package <- pieces[[1]]
  generic <- pieces[[2]]
  caller <- parent.frame()
  get_method_env <- function() {
    top <- topenv(caller)
    if (isNamespace(top)) {
      asNamespace(environmentName(top))
    }
    else {
      caller
    }
  }
  get_method <- function(method, env) {
    if (is.null(method)) {
      get(paste0(generic, ".", class), envir = get_method_env())
    }
    else {
      method
    }
  }
  method_fn <- get_method(method)
  stopifnot(is.function(method_fn))
  setHook(packageEvent(package, "onLoad"), function(...) {
    ns <- asNamespace(package)
    method_fn <- get_method(method)
    registerS3method(generic, class, method_fn, envir = ns)
  })
  if (!isNamespaceLoaded(package)) {
    return(invisible())
  }
  envir <- asNamespace(package)
  if (exists(generic, envir)) {
    registerS3method(generic, class, method_fn, envir = envir)
  }
  invisible()
}

# sandwich ----

bread.eglm <- function(x, ...) {
  if (!is.null(x$na.action)) class(x$na.action) <- "omit"
  sx <- summary(x)
  wres <- as.vector(residuals(x, "working")) * weights(x, "working")
  dispersion <- if (x$family$family %in% c("poisson", "binomial")) {
    1
  } else {
    sum(wres^2) / sum(weights(x, "working"))
  }
  return(sx$cov.unscaled * as.vector(sum(sx$df[1L:2L])) * dispersion)
}

bread.elm <- function(x, ...) {
  if(!is.null(x$na.action)) class(x$na.action) <- "omit"
  sx <- summary(x)
  return(sx$cov.unscaled * as.vector(sum(sx$df[1L:2L])))
}

# broom ----

tidy.glm <- function(x, conf.int = FALSE, conf.level = .95,
                     exponentiate = FALSE, ...) {
  # very similar to broom:::tidy.lm but using base
  stopifnot(any(class(x) %in% c("elm","lm")))
  ret <- as.data.frame(summary(x)$coefficients)
  rownames(ret) <- NULL
  ret <- cbind(rownames(summary(x)$coefficients), ret)
  colnames(ret) <- c("term", "estimate", "std.error", "statistic", "p.value")

  coefs <- as.data.frame(coef(x))
  rownames(coefs) <- NULL
  coefs <- cbind(names(coef(x)), coefs)
  colnames(coefs) <- c("term","estimate")

  ret <- merge(coefs, ret, by = c("term", "estimate"))
  if (exponentiate) ret$estimate <- exp(ret$estimate)

  if (conf.int) {
    ci <- as.data.frame(confint(x, level = conf.level))
    rownames(ci) <- NULL
    ci <- cbind(as.character(coefs$term), ci)
    colnames(ci) <- c("term", "conf.low", "conf.high")

    ret <- merge(ret, ci, by = "term")

    if (exponentiate) {
      ret$conf.low <- exp(ret$conf.low)
      ret$conf.high <- exp(ret$conf.high)
    }
  }

  ret
}

tidy.elm <- function(x, conf.int = FALSE, conf.level = 0.95, ...) {
  # very similar to broom:::tidy.lm but using base
  stopifnot(any(class(x) %in% c("elm","lm")))
  ret <- as.data.frame(summary(x)$coefficients)
  rownames(ret) <- NULL
  ret <- cbind(rownames(summary(x)$coefficients), ret)
  colnames(ret) <- c("term", "estimate", "std.error", "statistic", "p.value")

  coefs <- as.data.frame(coef(x))
  rownames(coefs) <- NULL
  coefs <- cbind(names(coef(x)), coefs)
  colnames(coefs) <- c("term","estimate")

  ret <- merge(coefs, ret, by = c("term", "estimate"))

  if (conf.int) {
    ci <- as.data.frame(confint(x, level = conf.level))
    rownames(ci) <- NULL
    ci <- cbind(as.character(coefs$term), ci)
    colnames(ci) <- c("term", "conf.low", "conf.high")

    ret <- merge(ret, ci, by = "term")
  }

  ret
}

##################################################################################################################################
#### augment.elm
##################################################################################################################################
augment.elm <- function(x, data = model.frame(x), newdata = NULL,
                        se_fit = FALSE, interval = c("none", "confidence", "prediction"), ...) {
  stopifnot(any(class(x) %in% c("elm","lm")))

  interval <- match.arg(interval)
  df <- augment_newdata(x, data, newdata, se_fit, interval)

  if (is.null(newdata)) {
    tryCatch({
      infl <- influence(x, do.coef = FALSE)
      df <- add_hat_sigma_cols(df, x, infl)
    },
    error = data_error
    )
  }
  df
}

##################################################################################################################################
#### glance.elm
##################################################################################################################################

glance.elm <- function(x, ...) {
  stopifnot(any(class(x) %in% c("elm","lm")))

  # Check whether the model was fitted with only an intercept, in which
  # case drop the fstatistic related column
  int_only <- nrow(summary(x)$coefficients) == 1

  with(
    summary(x),
    data.frame(
      r.squared = r.squared,
      adj.r.squared = adj.r.squared,
      sigma = sigma,
      statistic = if(!int_only) {fstatistic["value"]} else {NA_real_},
      p.value = if(!int_only) {
        pf(
          fstatistic["value"],
          fstatistic["numdf"],
          fstatistic["dendf"],
          lower.tail = FALSE
        )
      } else {NA_real_},
      df = if(!int_only) {fstatistic["numdf"]} else {NA_real_},
      logLik = as.numeric(stats::logLik(x)),
      AIC = stats::AIC(x),
      BIC = stats::BIC(x),
      deviance = stats::deviance(x),
      df.residual = df.residual(x),
      nobs = stats::nobs(x)
    )
  )
}

##################################################################################################################################
#### augment.glm
##################################################################################################################################
augment.glm <- function(x,
                        data = model.frame(x),
                        newdata = NULL,
                        type.predict = c("link", "response", "terms"),
                        type.residuals = c("deviance", "pearson"),
                        se_fit = FALSE, ...) {
  warn_on_appropriated_glm_class(x)
  stopifnot(any(class(x) %in% c("elm","lm")))

  type.predict <- match.arg(type.predict)
  type.residuals <- match.arg(type.residuals)

  df <- if (is.null(newdata)) data else newdata
  df <- as_augment_data_frame(df)

  # don't use augment_newdata here; don't want raw/response residuals in .resid
  if (se_fit) {
    pred_obj <- predict(x, newdata, type = type.predict, se.fit = TRUE)
    df$.fitted <- unname(pred_obj$fit)
    df$.se.fit <- unname(pred_obj$se.fit)
  } else {
    df$.fitted <- unname(predict(x, newdata, type = type.predict))
  }

  if (is.null(newdata)) {
    tryCatch({
      infl <- influence(x, do.coef = FALSE)
      df$.resid <- unname(residuals(x, type = type.residuals))
      df$.std.resid <- unname(rstandard(x, infl = infl, type = type.residuals))
      df <- add_hat_sigma_cols(df, x, infl)
      df$.cooksd <- unname(stats::cooks.distance(x, infl = infl))
    },
    error = data_error
    )
  }

  df

}

##################################################################################################################################
#### glance.glm
##################################################################################################################################

glance.glm <- function (x, ...) {
  warn_on_appropriated_glm_class(x)
  stopifnot(any(class(x) %in% c("elm","lm")))

  as_glance_data_frame(
    null.deviance = x$null.deviance,
    df.null = x$df.null,
    logLik = as.numeric(stats::logLik(x)),
    AIC = stats::AIC(x),
    BIC = stats::BIC(x),
    deviance = stats::deviance(x),
    df.residual = stats::df.residual(x),
    nobs = stats::nobs(x),
    na_types = "rirrrrii"
  )
}
