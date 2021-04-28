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
