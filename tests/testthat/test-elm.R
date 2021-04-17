# Regression ----

test_that("elm returns the same regression output as lm", {
  m1 <- lm(mpg ~ wt + am, data = mtcars)
  m2 <- elm(mpg ~ wt + am, data = mtcars, reduce = T)
  m3 <- elm(mpg ~ wt + am, data = mtcars, reduce = F)

  expect_equal(m1$coefficients, m2$coefficients)
  expect_equal(m1$residuals, m2$residuals)
  # expect_equal(m1$effects, m2$effects)
  expect_equal(m1$rank, m2$rank)
  expect_equal(m1$fitted.values, m2$fitted.values)
  expect_equal(m1$assign, m2$assign)
  # the QR decomposition is different by definition
  # expect_equal(m1$qr$qr, m2$qr$qr)
  # expect_equal(m1$qr$qraux, m2$qr$qraux)
  expect_equal(m1$qr$pivot, m2$qr$pivot)
  expect_equal(m1$qr$tol, m2$qr$tol)
  expect_equal(m1$qr$rank, m2$qr$rank)
  expect_equal(m1$df.residual, m2$df.residual)
  expect_equal(m1$xlevels, m2$xlevels)
  expect_equal(m1$call$formula, m2$call$formula)
  expect_equal(m1$call$data, m2$call$data)
  expect_equal(m1$terms, m2$terms)
  expect_equal(m1$model, m2$model)

  expect_equal(m1$coefficients, m3$coefficients)
  expect_equal(m1$residuals, m3$residuals)
  expect_equal(m1$effects, m3$effects)
  expect_equal(m1$rank, m3$rank)
  expect_equal(m1$fitted.values, m3$fitted.values)
  expect_equal(m1$assign, m3$assign)
  expect_equal(m1$qr$qr, m3$qr$qr)
  expect_equal(m1$qr$qraux, m3$qr$qraux)
  expect_equal(m1$qr$pivot, m3$qr$pivot)
  expect_equal(m1$qr$tol, m3$qr$tol)
  expect_equal(m1$qr$rank, m3$qr$rank)
  expect_equal(m1$df.residual, m3$df.residual)
  expect_equal(m1$xlevels, m3$xlevels)
  expect_equal(m1$call$formula, m3$call$formula)
  expect_equal(m1$call$data, m3$call$data)
  expect_equal(m1$terms, m3$terms)
  expect_equal(m1$model, m3$model)

  expect_equal(
    predict(m1, newdata = mtcars, type = "response"),
    predict(m2, newdata = mtcars, type = "response")
  )

  expect_equal(
    predict(m1, newdata = mtcars, type = "response"),
    predict(m3, newdata = mtcars, type = "response")
  )
})

# Design matrix ----

test_that("elm returns the same design matrix as lm", {
  m1 <- elm(mpg ~ wt, data = mtcars, x = TRUE)
  m2 <- lm(mpg ~ wt, data = mtcars, x = TRUE)
  expect_equal(m1$x, m2$x)
})
