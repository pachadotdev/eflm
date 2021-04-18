test_that("elm returns the same regression output as lm with formulas", {
  m1 <- lm(mpg ~ I(wt^2) + log(cyl), data = mtcars)
  m2 <- elm(mpg ~ I(wt^2) + log(cyl), data = mtcars)

  expect_equal(m2$coefficients, m1$coefficients)
  expect_equal(m2$residuals, m1$residuals)
  expect_equal(m2$fitted.values, m1$fitted.values)
  expect_equal(m2$rank, m1$rank)
  expect_equal(m2$df.residual, m1$df.residual)
  expect_equal(m2$call$formula, m1$call$formula)
  expect_equal(m2$call$data, m1$call$data)
  expect_equal(m2$terms, m1$terms)
  expect_equal(m2$model, m1$model)

  expect_equal(
    predict(m1, newdata = mtcars, type = "response"),
    predict(m2, newdata = mtcars, type = "response")
  )
})
