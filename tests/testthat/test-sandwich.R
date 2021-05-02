# LM ----

patrick::with_parameters_test_that("elm + vcovCL is the sane the same as lm:", {
  m1 <- lm(model, data = mtcars)
  m2 <- elm(model, data = mtcars, reduce = reduce)

  vcov_m1 <- sandwich::vcovCL(m1, cluster = eval(m1$call$data)[, "cyl"], type = "HC1")
  vcov_m2 <- sandwich::vcovCL(m2, cluster = eval(m2$call$data)[, "cyl"], type = "HC1")

  expect_equal(vcov_m2, vcov_m1)
},
.cases = elm_cases()
)

test_that("elm + vcovBS return the same as lm + vcovBS", {
  # vcovBS uses bootstrap so I need a seed to compare two results!
  with_seed <- function(seed, code) {
    code <- substitute(code)
    set.seed(seed)
    eval.parent(code)
  }

  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- eflm::elm(mpg ~ wt, data = mtcars, reduce = F)
  m3 <- eflm::elm(mpg ~ wt, data = mtcars, reduce = T)

  vcov_m1 <- with_seed(1813, sandwich::vcovBS(m1, cluster = NULL))
  vcov_m2 <- with_seed(1813, sandwich::vcovBS(m2, cluster = NULL))
  vcov_m3 <- with_seed(1813, sandwich::vcovBS(m3, cluster = NULL))

  expect_equal(vcov_m2, vcov_m1)
  expect_equal(vcov_m3, vcov_m1)

  vcov_m1 <- with_seed(1813, sandwich::vcovBS(m1, cluster = eval(m1$call$data)[, "cyl"]))
  vcov_m2 <- with_seed(1813, sandwich::vcovBS(m2, cluster = eval(m2$call$data)[, "cyl"]))
  vcov_m3 <- with_seed(1813, sandwich::vcovBS(m3, cluster = eval(m3$call$data)[, "cyl"]))

  expect_equal(vcov_m2, vcov_m1)
  expect_equal(vcov_m3, vcov_m1)
})

# GLM ----

patrick::with_parameters_test_that("eglm + vcovCL is the sane the same as glm:", {
  m1 <- glm(model, data = mtcars, family = family)
  m2 <- eglm(model, data = mtcars, family = family, reduce = reduce)

  vcov_m1 <- sandwich::vcovCL(m1, cluster = NULL, type = "HC0")
  vcov_m2 <- sandwich::vcovCL(m2, cluster = NULL, type = "HC0")

  expect_equal(vcov_m2, vcov_m1)

  vcov_m1 <- sandwich::vcovCL(m1, cluster = eval(m1$call$data)[, "cyl"], type = "HC0")
  vcov_m2 <- sandwich::vcovCL(m2, cluster = eval(m2$call$data)[, "cyl"], type = "HC0")

  expect_equal(vcov_m2, vcov_m1)
},
.cases = eglm_cases()
)

test_that("eglm + vcovBS return the same as glm + vcovBS", {
  # vcovBS uses bootstrap so I need a seed to compare two results!
  with_seed <- function(seed, code) {
    code <- substitute(code)
    set.seed(seed)
    eval.parent(code)
  }

  m1 <- glm(mpg ~ wt, family = gaussian, data = mtcars)
  m2 <- eflm::eglm(mpg ~ wt, family = gaussian, data = mtcars, reduce = F)
  m3 <- eflm::eglm(mpg ~ wt, family = gaussian, data = mtcars, reduce = T)

  vcov_m1 <- with_seed(1813, sandwich::vcovBS(m1, cluster = NULL))
  vcov_m2 <- with_seed(1813, sandwich::vcovBS(m2, cluster = NULL))
  vcov_m3 <- with_seed(1813, sandwich::vcovBS(m3, cluster = NULL))

  expect_equal(vcov_m2, vcov_m1)
  expect_equal(vcov_m3, vcov_m1)

  vcov_m1 <- with_seed(1813, sandwich::vcovBS(m1, cluster = eval(m1$call$data)[, "cyl"]))
  vcov_m2 <- with_seed(1813, sandwich::vcovBS(m2, cluster = eval(m2$call$data)[, "cyl"]))
  vcov_m3 <- with_seed(1813, sandwich::vcovBS(m3, cluster = eval(m3$call$data)[, "cyl"]))

  expect_equal(vcov_m2, vcov_m1)
  expect_equal(vcov_m3, vcov_m1)
})
