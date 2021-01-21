# Dynamically exported, see zzz.R
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
