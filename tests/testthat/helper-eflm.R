mdl <- function(f) {
  if (!any(f %in% c("poisson", "binomial", "quasibinomial"))) {
    "mpg ~ wt + am"
  } else {
    "am ~ wt + mpg"
  }
}
