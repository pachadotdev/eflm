# Dynamically exported, see zzz.R

# taken from broom:::
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

# taken from broom::: but adapted to use base
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

