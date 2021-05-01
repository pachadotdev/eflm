fmly <- c("gaussian", "inverse.gaussian", "Gamma", "binomial", "quasibinomial",
          "poisson", "quasipoisson", "quasi")
wgts <- list(NULL)
tst <- c("none", "F", "Chisq", "LRT", "Rao")

mdl <- function(f) {
  if (!any(f %in% c("poisson", "binomial", "quasibinomial"))) {
    "mpg ~ wt + am"
  } else {
    "am ~ wt + mpg"
  }
}

make_eglm_cases <- function() {
  d <- data.frame(
    test_name = fmly,
    family = fmly,
    model = sapply(fmly, mdl),
    reduce = T
  )

  d2 <- d
  d2$reduce <- F

  return(rbind(d, d2))
}

make_elm_cases <- function() {
  d <- data.frame(
    test_name = "gaussian",
    model = rep("am ~ wt + mpg", 2),
    reduce = c(T,F)
  )

  return(d)
}
