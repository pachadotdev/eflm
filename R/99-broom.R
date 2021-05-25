# broom GLM ----

tidy.eglm <- function(x, conf.int = FALSE, conf.level = .95,
                     exponentiate = FALSE) {
  # very similar to broom:::tidy.lm but using just tibble
  stopifnot(any(class(x) %in% c("eglm","glm")))

  # ret <- tibble::as_tibble(summary(x)$coefficients, rownames = "term")
  # colnames(ret) <- c("term", "estimate", "std.error", "statistic", "p.value")
  ret <- as.data.frame(summary(x)$coefficients)
  rownames(ret) <- NULL
  ret <- cbind(rownames(summary(x)$coefficients), ret)
  colnames(ret) <- c("term", "estimate", "std.error", "statistic", "p.value")

  # coefs <- tibble::enframe(coef(x), name = "term", value = "estimate")
  # ret <- merge(coefs, ret, by = c("term", "estimate"))
  coefs <- as.data.frame(coef(x))
  rownames(coefs) <- NULL
  coefs <- cbind(names(coef(x)), coefs)
  colnames(coefs) <- c("term","estimate")

  if (exponentiate) {
    ret$estimate <- exp(ret$estimate)
  }

  if (conf.int) {
    ci <- tibble::as_tibble(confint(x, level = conf.level), rownames = "term")
    colnames(ci) <- c("term", "conf.low", "conf.high")

    ret <- merge(ret, ci, by = "term")

    if (exponentiate) {
      ret$conf.low <- exp(ret$conf.low)
      ret$conf.high <- exp(ret$conf.high)
    }
  }

  return(ret)
}

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

# broom LM ----

tidy.elm <- function(x, conf.int = FALSE, conf.level = 0.95, ...) {
  # very similar to broom:::tidy.lm but using base
  stopifnot(any(class(x) %in% c("elm","lm")))

  # ret <- tibble::as_tibble(summary(x)$coefficients, rownames = "term")
  # colnames(ret) <- c("term", "estimate", "std.error", "statistic", "p.value")
  ret <- as.data.frame(summary(x)$coefficients)
  rownames(ret) <- NULL
  ret <- cbind(rownames(summary(x)$coefficients), ret)
  colnames(ret) <- c("term", "estimate", "std.error", "statistic", "p.value")

  # coefs <- tibble::enframe(coef(x), name = "term", value = "estimate")
  coefs <- as.data.frame(coef(x))
  rownames(coefs) <- NULL
  coefs <- cbind(names(coef(x)), coefs)
  colnames(coefs) <- c("term","estimate")

  ret <- merge(coefs, ret, by = c("term", "estimate"))

  if (conf.int) {
    ci <- tibble::as_tibble(confint(x, level = conf.level), rownames = "term")
    colnames(ci) <- c("term", "conf.low", "conf.high")

    ret <- merge(ret, ci, by = "term")
  }

  ret
}

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

# utilities ----

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

has_rownames <- function(df) {
  any(row.names(df) != as.character(1:nrow(df)))
}

add_column <- function(df) {
  df <- as.data.frame(df)
  col_to_add <- rownames(df)
  rownames(df) <- NULL
  df <- cbind(col_to_add, df)
  names(df)[names(df) == "col_to_add"] <- ".rownames"
  df
}

as_augment_data_frame <- function(data) {
  if(inherits(data, "matrix") & is.null(colnames(data))) {
    stop("The supplied `data`/`newdata` argument was an unnamed matrix. ",
         "Please supply a matrix or dataframe with column names.")
  }

  tryCatch(
    df <- as.data.frame(data),
    error = function(cnd) {
      stop("Could not coerce data to `tibble`. Try explicitly passing a",
           "dataset to either the `data` or `newdata` argument.",
           call. = FALSE
      )
    }
  )

  if(has_rownames(data)) {
    df <- add_column(df)
  }

  df
}

response <- function(object, newdata = NULL) {
  model.response(model.frame(terms(object), data = newdata, na.action = na.pass))
}

augment_newdata <- function(x, data, newdata, .se_fit, interval = NULL, ...) {
  passed_newdata <- !is.null(newdata)
  if(passed_newdata) {
    df <- newdata
  } else {
    df <- data
  }
  df <- as_augment_data_frame(df)

  response_var_in_newdata <- is.element(all.vars(x$call)[[1]],
                                        names(df))

  # NOTE: It is important use predict(x, newdata = newdata) rather than
  # predict(x, newdata = df). This is to avoid an edge case breakage
  # when augment is called with no data argument, so that data is
  # model.frame(x). When data = model.frame(x) and the model formula
  # contains a term like `log(x)`, the predict method will break. Luckily,
  # predict(x, newdata = NULL) works perfectly well in this case.
  #
  # The current code relies on predict(x, newdata = NULL) functioning
  # equivalently to predict(x, newdata = data). An alternative would be to use
  # fitted(x) instead, although this may not play well with missing data,
  # and may behave like na.action = na.omit rather than na.action = na.pass.

  # This helper *should not* be used for predict methods that do not have
  # an na.pass argument

  if(.se_fit) {
    pred_obj <- predict(x, newdata = newdata, na.action = na.pass, se.fit = .se_fit, interval = interval, ...)
    if(is.null(interval) || interval == "none") {
      df$.fitted <- unname(pred_obj$fit)
    } else {
      df$.fitted <- pred_obj$fit[, "fit"]
      df$.lower <- pred_obj$fit[, "lwr"]
      df$.upper <- pred_obj$fit[, "upr"]
    }

    se_idx <- which(names(pred_obj) %in% c("se.fit", "se"))
    df$.se.fit <- pred_obj[[se_idx]]
  } else if(!is.null(interval) && interval != "none") {
    pred_obj <- predict(x, newdata = newdata, na.action = na.pass, se.fit = FALSE, interval = interval, ...)
    df$.fitted <- pred_obj[, "fit"]
    df$.lower <- pred_obj[, "lwr"]
    df$.upper <- pred_obj[, "upr"]
  } else if (passed_newdata) {
    if (is.null(interval) || interval=="none") {
      unname(df$.fitted <- predict(x, newdata = newdata, na.action = na.pass, ...))
    } else {
      pred_obj <- predict(x, newdata = newdata, na.action = na.pass, interval = interval, ...)
      df$.fitted <- pred_obj$fit[, "fit"]
      df$.lower <- pred_obj$fit[, "lwr"]
      df$.upper <- pred_obj$fit[, "upr"]
    }
  } else {
    if (is.null(interval) || interval=="none") {
      unname(df$.fitted <- predict(x, na.action = na.pass, ...))
    } else {
      pred_obj <- predict(x, newdata = newdata, na.action = na.pass, interval = interval, ...)
      df$.fitted <- pred_obj$fit[, "fit"]
      df$.lower <- pred_obj$fit[, "lwr"]
      df$.upper <- pred_obj$fit[, "upr"]
    }
  }

  resp <- try(response(x, df), silent = TRUE)

  if(!is.null(resp) && is.numeric(resp)) {
    df$.resid <- unname((resp - df$.fitted))
  }

  df
}

# in weighted regressions, influence measures should be zero for
# data points with zero weight
# helper for augment.lm and augment.glm
add_hat_sigma_cols <- function(df, x, infl) {
  df$.hat <- 0
  df$.sigma <- 0
  df$.cooksd <- 0
  df$.std.resid <- NA

  w <- x$weights
  nonzero_idx <- if (is.null(w)) seq_along(df$.hat) else which(w != 0)

  df$.hat[nonzero_idx] <- unname(infl$hat)
  df$.sigma[nonzero_idx] <- unname(infl$sigma)
  df$.std.resid[nonzero_idx] <- unname(rstandard(x, infl = infl))
  df$.cooksd[nonzero_idx] <- unname(cooks.distance(x, infl = infl))
  df
}

data_error <- function(cnd) {
  stop(
    "Must specify either `data` or `newdata` argument.",
    call. = FALSE
  )
}

na_types_dict <- list("r" = NA_real_,
                      "i" = NA_integer_,
                      "c" = NA_character_,
                      "l" = NA)

parse_na_types <- function(s) {
  positions <- unlist(lapply(strsplit(s, split = ""), match, table = names(na_types_dict)))

  unname(unlist(na_types_dict[positions]))
}

as_glance_data_frame <- function(..., na_types) {
  cols <- list(...)

  if(length(cols) != nchar(na_types)) {
    stop(
      "The number of columns provided does not match the number of ",
      "column types provided."
    )
  }

  na_types_long <- parse_na_types(na_types)

  entries <- mapply(function(.x, .y) {if (length(.x) == 0) .y else .x},
                    cols,
                    na_types_long)

  names_of_cols <- names(entries)
  intermediate_matrix <- matrix(entries, nrow = 1)
  data_frame_single_row <- data.frame(intermediate_matrix)
  colnames(data_frame_single_row) <- names_of_cols
  data_frame_single_row
}

# warn_on_appropriated_glm_class <- function(x) {
#   warn_on_glm2(x)
#   warn_on_stanreg(x)
#
#   invisible(TRUE)
# }
#
# # the output of glm2::glm2 has the same class as objects outputted
# # by stats::glm2. glm2 outputs are currently not supported (intentionally)
# # so warn that output is not maintained.
# warn_on_glm2 <- function(x) {
#   if (!is.null(x$method)) {
#     if (x$method == "glm.fit2") {
#       warning("The supplied model object seems to be outputted from the glm2 ",
#               "package. Tidiers for glm2 output are currently not ",
#               "maintained; please use caution in interpreting broom output.")
#     }
#   }
#
#   invisible(TRUE)
# }
#
# # stanreg objects subclass glm, glm tidiers error out (uninformatively),
# # and the maintained stanreg tidiers live in broom.mixed.
# warn_on_stanreg <- function(x) {
#   if (!is.null(x$stan_function)) {
#     stop("The supplied model object seems to be outputted from the rstanarm ",
#          "package. Tidiers for mixed model output now live in the broom.mixed package.")
#   }
#
#   invisible(TRUE)
# }
