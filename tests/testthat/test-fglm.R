test_that("gaussian with one variable is equivalent to glm", {
  m1 <- glm(mpg ~ wt, family = gaussian(), data = mtcars)
  m2 <- fglm(mpg ~ wt, family = gaussian(), data = mtcars)

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
  expect_equal(as.vector(m1$y), m2$y)
  expect_equal(m1$call$formula, m2$call$formula)
  expect_equal(m1$call$family, m2$call$family)
  expect_equal(m1$call$data, m2$call$data)

  expect_equal(
    predict(m1, newdata = mtcars, type = "link"),
    predict(m2, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m1, newdata = mtcars, type = "response"),
    predict(m2, newdata = mtcars, type = "response")
  )
})
