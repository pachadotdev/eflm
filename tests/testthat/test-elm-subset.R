test_that("subset works and returns the same results for lm and elm", {
  m1 <- lm(log(mpg) ~ log(wt), data = mtcars, subset = (cyl == 4 & am == 1))
  m2 <- elm(log(mpg) ~ log(wt), data = mtcars, subset = (cyl == 4 & am == 1))

  expect_equal(m1$coefficients, m2$coefficients)
  expect_equal(m1$rank, m2$rank)
  expect_equal(m1$df.residual, m2$df.residual)
  expect_equal(m1$call$formula, m2$call$formula)
  expect_equal(m1$call$data, m2$call$data)
  expect_equal(m1$call$formula, m2$call$formula)
  expect_equal(m1$terms, m2$terms)
  expect_equal(m1$model, m2$model)

  # Fixed with Ben Raymond suggestions, rOpenSci
  expect_equal(m1$residuals, m2$residuals)
  expect_equal(m1$fitted.values, m2$fitted.values)
  expect_equal(
    predict(m1, newdata = mtcars, type = "response"),
    predict(m2, newdata = mtcars, type = "response")
  )
})
