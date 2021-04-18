# Regression ----

test_that("elm returns the same regression output as lm", {
  m1 <- lm(mpg ~ wt + am, data = mtcars)
  m2 <- elm(mpg ~ wt + am, data = mtcars, reduce = F)
  m3 <- elm(mpg ~ wt + am, data = mtcars, reduce = T)

  expect_equal(m2$coefficients, m1$coefficients)
  expect_equal(m2$residuals, m1$residuals)
  expect_equal(m2$rank, m1$rank)
  expect_equal(m2$fitted.values, m1$fitted.values)
  expect_equal(m2$assign, m1$assign)
  expect_equal(m2$effects, m1$effects)
  expect_equal(m2$qr$qr, m1$qr$qr)
  expect_equal(m2$qr$qraux, m1$qr$qraux)
  expect_equal(m2$qr$pivot, m1$qr$pivot)
  expect_equal(m2$qr$tol, m1$qr$tol)
  expect_equal(m2$qr$rank, m1$qr$rank)
  expect_equal(m2$df.residual, m1$df.residual)
  expect_equal(m2$xlevels, m1$xlevels)
  expect_equal(m2$call$formula, m1$call$formula)
  expect_equal(m2$call$data, m1$call$data)
  expect_equal(m2$terms, m1$terms)
  expect_equal(m2$model, m1$model)

  expect_equal(m3$coefficients, m1$coefficients)
  expect_equal(m3$residuals, m1$residuals)
  expect_equal(m3$rank, m1$rank)
  expect_equal(m3$fitted.values, m1$fitted.values)
  expect_equal(m3$assign, m1$assign)
  # the QR decomposition is different by definition
  # expect_equal(m3$effects, m1$effects)
  # expect_equal(m3$qr$qr, m1$qr$qr)
  # expect_equal(m3$qr$qraux, m1$qr$qraux)
  expect_equal(m3$qr$pivot, m1$qr$pivot)
  expect_equal(m3$qr$tol, m1$qr$tol)
  expect_equal(m3$qr$rank, m1$qr$rank)
  expect_equal(m3$df.residual, m1$df.residual)
  expect_equal(m3$xlevels, m1$xlevels)
  expect_equal(m3$call$formula, m1$call$formula)
  expect_equal(m3$call$data, m1$call$data)
  expect_equal(m3$terms, m1$terms)
  expect_equal(m3$model, m1$model)

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
  m1 <- lm(mpg ~ wt, data = mtcars, x = TRUE)
  m2 <- elm(mpg ~ wt, data = mtcars, x = TRUE, reduce = FALSE)
  m3 <- elm(mpg ~ wt, data = mtcars, x = TRUE, reduce = TRUE)
  expect_equal(m2$x, m1$x)
  expect_equal(m3$x, m1$x)
})
