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

eglm_cases <- function() {
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

elm_cases <- function() {
  d <- data.frame(
    test_name = "gaussian",
    model = rep("am ~ wt + mpg", 2),
    reduce = c(T,F)
  )

  return(d)
}

elm_add1_cases <- function() {
  d <- elm_cases()

  d2 <- data.frame(
    model = rep(unique(d$model), 3),
    test = c("none", "Chisq", "F")
  )

  return(merge(d, d2))
}

eglm_add1_cases <- function() {
  d <- eglm_cases()

  d2 <- data.frame(
    model = rep(unique(d$model), 5),
    test = c("none", "Rao", "LRT", "Chisq", "F")
  )

  d <- merge(d, d2)

  d <- d[!(d$family == "binomial" & d$test == "F"), ]
  d <- d[!(d$family == "poisson" & d$test == "F"), ]

  return(d)
}
