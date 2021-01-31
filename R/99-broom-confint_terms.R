# Dynamically exported, see zzz.R

# taken from broom::: (makes tidy.bglm work)
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
