# Dynamically exported, see zzz.R

# taken from broom::: but adapted to use (mostly) base
tidy.elm <- function(x, conf.int = FALSE, conf.level = 0.95, ...) {
  ret <- tibble::as_tibble(summary(x)$coefficients, rownames = "term")
  colnames(ret) <- c("term", "estimate", "std.error", "statistic", "p.value")
  coefs <- tibble::enframe(stats::coef(x), name = "term", value = "estimate")
  ret <- merge(coefs, ret, by = c("term", "estimate"))
  if (conf.int) {
    ci <- confint_terms(x, level = conf.level)
    ret <- merge(ret, ci, by = "term")
  }
  ret
}
