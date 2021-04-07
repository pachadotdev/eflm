# Gaussian ----

test_that("eglm (gaussian) + cholesky singularity.method == glm", {
  m1 <- glm(mpg ~ wt + am, family = gaussian, data = mtcars)
  m2 <- eglm(mpg ~ wt + am, family = gaussian, data = mtcars, singularity.method = "Cholesky")

  expect_equal(
    predict(m1, newdata = mtcars, type = "link"),
    predict(m2, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m1, newdata = mtcars, type = "response"),
    predict(m2, newdata = mtcars, type = "response")
  )
})

# Inverse-Gaussian ----

test_that("eglm (inverse.gaussian) == glm", {
  m1 <- glm(mpg ~ wt + am, family = inverse.gaussian, data = mtcars)
  m2 <- eglm(mpg ~ wt + am, family = inverse.gaussian, data = mtcars)

  expect_equal(
    predict(m1, newdata = mtcars, type = "link"),
    predict(m2, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m1, newdata = mtcars, type = "response"),
    predict(m2, newdata = mtcars, type = "response")
  )
})

# Gamma ----

test_that("eglm (gaussian) == glm", {
  m1 <- glm(mpg ~ wt + am, family = Gamma, data = mtcars)
  m2 <- eglm(mpg ~ wt + am, family = Gamma, data = mtcars)

  expect_equal(
    predict(m1, newdata = mtcars, type = "link"),
    predict(m2, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m1, newdata = mtcars, type = "response"),
    predict(m2, newdata = mtcars, type = "response")
  )
})

# Binomial ----

test_that("eglm (binomial) == glm", {
  m1 <- glm(am ~ wt + mpg, family = binomial, data = mtcars)
  m2 <- eglm(am ~ wt + mpg, family = binomial, data = mtcars)

  expect_equal(
    predict(m1, newdata = mtcars, type = "link"),
    predict(m2, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m1, newdata = mtcars, type = "response"),
    predict(m2, newdata = mtcars, type = "response")
  )
})

# Quasi-Binomial ----

test_that("eglm (quasibinomial) == glm", {
  m1 <- glm(am ~ wt + mpg, family = quasibinomial, data = mtcars)
  m2 <- eglm(am ~ wt + mpg, family = quasibinomial, data = mtcars)

  expect_equal(
    predict(m1, newdata = mtcars, type = "link"),
    predict(m2, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m1, newdata = mtcars, type = "response"),
    predict(m2, newdata = mtcars, type = "response")
  )
})

# Poisson ----

test_that("eglm (poisson) == glm", {
  m1 <- glm(am ~ wt + mpg, family = poisson, data = mtcars)
  m2 <- eglm(am ~ wt + mpg, family = poisson, data = mtcars)

  expect_equal(
    predict(m1, newdata = mtcars, type = "link"),
    predict(m2, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m1, newdata = mtcars, type = "response"),
    predict(m2, newdata = mtcars, type = "response")
  )
})

# Quasi-Poisson ----

test_that("eglm (quasipoisson) == glm", {
  m1 <- glm(mpg ~ wt + am, family = quasipoisson, data = mtcars)
  m2 <- eglm(mpg ~ wt + am, family = quasipoisson, data = mtcars)

  expect_equal(
    predict(m1, newdata = mtcars, type = "link"),
    predict(m2, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m1, newdata = mtcars, type = "response"),
    predict(m2, newdata = mtcars, type = "response")
  )
})

# Quasi ----

test_that("eglm (quasi) == glm", {
  m1 <- glm(mpg ~ wt + am, family = quasi, data = mtcars)
  m2 <- eglm(mpg ~ wt + am, family = quasi, data = mtcars)

  expect_equal(
    predict(m1, newdata = mtcars, type = "link"),
    predict(m2, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m1, newdata = mtcars, type = "response"),
    predict(m2, newdata = mtcars, type = "response")
  )
})

# NULL warnings ----

test_that("predict.eglm fails with NULL fitted values + newdata", {
  m1 <- eglm(mpg ~ wt + am, family = gaussian, data = mtcars)
  m2 <- eglm(mpg ~ wt + am, family = gaussian, data = mtcars)

  expect_equal(predict(m1), predict(m2))
  m1$fitted.values <- NULL
  expect_warning(predict(m1, newdata = NULL))
})
