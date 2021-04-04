test_that("elm returns the same regression output as lm with formulas", {
  m1 <- lm(mpg ~ I(wt^2) + log(cyl), data = mtcars)
  m2 <- elm(mpg ~ I(wt^2) + log(cyl), data = mtcars)

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

test_that("eglm (gaussian) == glm with formulas", {
  m1 <- glm(mpg ~ I(wt^2) + log(cyl), family = gaussian(), data = mtcars)
  m2 <- eglm(mpg ~ I(wt^2) + log(cyl), family = gaussian(), data = mtcars)

  expect_equal(m1$coefficients, m2$coefficients)
  expect_equal(m1$residuals, m2$residuals)
  expect_equal(m1$fitted.values, m2$fitted.values)
  expect_equal(m1$family$family, m2$family$family)
  expect_equal(m1$family$link, m2$family$link)
  expect_equal(m1$linear.predictors, m2$linear.predictors)
  expect_equal(m1$deviance, m2$deviance)
  expect_equal(m1$aic, m2$aic)
  expect_equal(m1$null.deviance, m2$null.deviance)
  expect_equal(m1$df.residual, m2$df.residual)
  expect_equal(m1$df.null, m2$df.null)
  expect_equal(m1$y, m2$y)
  expect_equal(m1$call$formula, m2$call$formula)
  expect_equal(m1$call$family, m2$call$family)
  expect_equal(m1$call$data, m2$call$data)
  expect_equal(m1$qr$tol, m2$qr$tol)

  # CHECK THIS ----
  # expect_equal(m1$qr$qr, m2$qr$qr)
  # expect_equal(m1$qr$rank, m2$qr$rank)
  # expect_equal(m1$qr$qraux, m2$qr$qraux)
  # expect_equal(m1$qr$pivot, m2$qr$pivot)
})
