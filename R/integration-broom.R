# Dynamically exported, see zzz.R

# taken from broom:::
#' @importFrom stats confint
broom_confint_terms <- function (x, ...) {
  ellipsis::check_dots_used()
  ci <- suppressMessages(confint(x, ...))
  if (is.null(dim(ci))) {
    ci <- matrix(ci, nrow = 1)
    rownames(ci) <- names(coef(x))[1]
  }
  ci <- dplyr::as_tibble(ci, rownames = "term")
  names(ci) <- c("term", "conf.low", "conf.high")
  ci
}

# taken from broom:::
exponentiate <- function (data) {
  data <- dplyr::mutate_at(data, dplyr::vars("estimate"), exp)
  if ("conf.low" %in% colnames(data)) {
    data <- dplyr::mutate_at(data, dplyr::vars("conf.low", "conf.high"), exp)
  }
  data
}

tidy.fglm <- function (x, conf.int = FALSE, conf.level = 0.95, exponentiate = FALSE,
                       ...) {
  ret <- dplyr::as_tibble(summary(x)$coefficients, rownames = "term")
  colnames(ret) <- c("term", "estimate", "std.error", "statistic", "p.value")
  if (is.character(ret$p.value)) ret$p.value <- as.numeric(ret$p.value)
  if (conf.int) {
    ci <- broom_confint_terms(x, level = conf.level)
    ret <- dplyr::left_join(ret, ci, by = "term")
  }
  if (exponentiate) {
    ret <- exponentiate(ret)
  }
  ret
}
