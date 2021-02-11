# Regression ----

test_that("elm returns the same regression output as lm", {
  m1 <- lm(mpg ~ wt + am, data = mtcars)
  m2 <- elm(mpg ~ wt + am, data = mtcars)

  expect_equal(m1$coefficients, m2$coefficients)
  expect_equal(m1$residuals, m2$residuals)
  expect_equal(m1$fitted.values, m2$fitted.values)
  expect_equal(m1$rank, m2$rank)
  expect_equal(m1$df.residual, m2$df.residual)
  expect_equal(m1$call$formula, m2$call$formula)
  expect_equal(m1$call$data, m2$call$data)
  expect_equal(m1$terms, m2$terms)
  expect_equal(m1$model, m2$model)

  expect_equal(
    predict(m1, newdata = mtcars, type = "response"),
    predict(m2, newdata = mtcars, type = "response")
  )
})

test_that("elm with qr singularity method returns the same regression output as lm", {
  m1 <- lm(mpg ~ wt + am, data = mtcars)
  m2 <- elm(mpg ~ wt + am, data = mtcars, singularity.method = "qr")

  expect_equal(m1$coefficients, m2$coefficients)
  expect_equal(m1$residuals, m2$residuals)
  expect_equal(m1$fitted.values, m2$fitted.values)
  expect_equal(m1$rank, m2$rank)
  expect_equal(m1$df.residual, m2$df.residual)
  expect_equal(m1$call$formula, m2$call$formula)
  expect_equal(m1$call$data, m2$call$data)
  expect_equal(m1$terms, m2$terms)
  expect_equal(m1$model, m2$model)

  expect_equal(
    predict(m1, newdata = mtcars, type = "response"),
    predict(m2, newdata = mtcars, type = "response")
  )
})

test_that("elm with cholesky singularity method returns the same regression output as lm", {
  m1 <- lm(mpg ~ wt + am, data = mtcars)
  m2 <- elm(mpg ~ wt + am, data = mtcars, singularity.method = "Cholesky")

  expect_equal(m1$coefficients, m2$coefficients)
  expect_equal(m1$residuals, m2$residuals)
  expect_equal(m1$fitted.values, m2$fitted.values)
  expect_equal(m1$rank, m2$rank)
  expect_equal(m1$df.residual, m2$df.residual)
  expect_equal(m1$call$formula, m2$call$formula)
  expect_equal(m1$call$data, m2$call$data)
  expect_equal(m1$terms, m2$terms)
  expect_equal(m1$model, m2$model)

  expect_equal(
    predict(m1, newdata = mtcars, type = "response"),
    predict(m2, newdata = mtcars, type = "response")
  )
})

# Design matrix ----

test_that("elm returns the same design matrix as lm", {
  m1 <- elm(mpg ~ wt, data = mtcars, x = TRUE)
  m2 <- lm(mpg ~ wt, data = mtcars, x = TRUE)
  expect_equal(m1$x, m2$x)
})
