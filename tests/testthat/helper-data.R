fmly <- c("gaussian", "inverse.gaussian", "Gamma", "binomial", "quasibinomial",
          "poisson", "quasipoisson", "quasi")

wgts <- list(NULL)

mdl <- function(f) {
  if (!any(f %in% c("poisson", "binomial", "quasibinomial"))) {
    "mpg ~ wt"
  } else {
    "am ~ mpg"
  }
}

eglm_cases <- function() {
  d <- data.frame(
    family = fmly,
    model = sapply(fmly, mdl),
    reduce = T
  )

  d2 <- d
  d2$reduce <- F

  d <- rbind(d, d2)

  d$`.test_name` <- sprintf("model = %s - family = %s - reduce = %s",
                         d$model, d$family, d$reduce)

  return(d)
}

elm_cases <- function() {
  d <- data.frame(
    model = rep("am ~ mpg", 2),
    reduce = c(T,F)
  )

  d$`.test_name` <- sprintf("model = %s - family = %s - reduce = %s",
                         d$model, "gaussian", d$reduce)

  return(d)
}

elm_add1_cases <- function() {
  d <- elm_cases()

  d2 <- data.frame(
    model = rep(unique(d$model), 3),
    test = c("none", "Chisq", "F")
  )

  d <- merge(d, d2)

  d$`.test_name` <- sprintf("model = %s - family = %s - reduce = %s - test = %s",
                         d$model, "gaussian", d$reduce, d$test)

  return(d)
}
