# Gaussian ----

test_that("fglm (gaussian) + cholesky singularity.method == glm", {
  m1 <- glm(mpg ~ wt + am, family = gaussian(), data = mtcars)
  m2 <- fglm(mpg ~ wt + am, family = gaussian(), data = mtcars, singularity.method = "Cholesky")

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

test_that("fglm (inverse.gaussian) == glm", {
  m1 <- glm(mpg ~ wt + am, family = inverse.gaussian(), data = mtcars)
  m2 <- fglm(mpg ~ wt + am, family = inverse.gaussian(), data = mtcars)

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

test_that("fglm (gaussian) == glm", {
  m1 <- glm(mpg ~ wt + am, family = Gamma(), data = mtcars)
  m2 <- fglm(mpg ~ wt + am, family = Gamma(), data = mtcars)

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

test_that("fglm (binomial) == glm", {
  m1 <- glm(am ~ wt + mpg, family = binomial(), data = mtcars)
  m2 <- fglm(am ~ wt + mpg, family = binomial(), data = mtcars)

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

test_that("fglm (quasibinomial) == glm", {
  m1 <- glm(am ~ wt + mpg, family = quasibinomial(), data = mtcars)
  m2 <- fglm(am ~ wt + mpg, family = quasibinomial(), data = mtcars)

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

test_that("fglm (poisson) == glm", {
  m1 <- glm(am ~ wt + mpg, family = poisson(), data = mtcars)
  m2 <- fglm(am ~ wt + mpg, family = poisson(), data = mtcars)

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

test_that("fglm (quasipoisson) == glm", {
  m1 <- glm(mpg ~ wt + am, family = quasipoisson(), data = mtcars)
  m2 <- fglm(mpg ~ wt + am, family = quasipoisson(), data = mtcars)

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

test_that("fglm (quasi) == glm", {
  m1 <- glm(mpg ~ wt + am, family = quasi(), data = mtcars)
  m2 <- fglm(mpg ~ wt + am, family = quasi(), data = mtcars)

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

test_that("predict.fglm fails with NULL fitted values + newdata", {
  m1 <- fglm(mpg ~ wt + am, family = gaussian(), data = mtcars)
  m2 <- fglm(mpg ~ wt + am, family = gaussian(), data = mtcars)

  expect_equal(predict(m1), predict(m2))
  m1$fitted.values <- NULL
  expect_warning(predict(m1, newdata = NULL))
})
