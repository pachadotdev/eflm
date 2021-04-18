test_that("subset works and returns the same results for lm and elm", {
  m1 <- lm(log(mpg) ~ log(wt), data = mtcars, subset = (cyl == 4 & am == 1))
  m2 <- elm(log(mpg) ~ log(wt), data = mtcars, subset = (cyl == 4 & am == 1), reduce = F)
  m3 <- elm(log(mpg) ~ log(wt), data = mtcars, subset = (cyl == 4 & am == 1), reduce = T)

  expect_equal(m2$coefficients, m1$coefficients)
  expect_equal(m2$rank, m1$rank)
  expect_equal(m2$df.residual, m1$df.residual)
  expect_equal(m2$call$formula, m1$call$formula)
  expect_equal(m2$call$data, m1$call$data)
  expect_equal(m2$call$formula, m1$call$formula)
  expect_equal(m2$terms, m1$terms)
  expect_equal(m2$model, m1$model)

  # Fixed with Ben Raymond suggestions, rOpenSci
  expect_equal(m2$residuals, m1$residuals)
  expect_equal(m2$fitted.values, m1$fitted.values)
  expect_equal(
    predict(m1, newdata = mtcars, type = "response"),
    predict(m2, newdata = mtcars, type = "response")
  )

  expect_equal(m3$coefficients, m1$coefficients)
  expect_equal(m3$rank, m1$rank)
  expect_equal(m3$df.residual, m1$df.residual)
  expect_equal(m3$call$formula, m1$call$formula)
  expect_equal(m3$call$data, m1$call$data)
  expect_equal(m3$call$formula, m1$call$formula)
  expect_equal(m3$terms, m1$terms)
  expect_equal(m3$model, m1$model)

  # Fixed with Ben Raymond suggestions, rOpenSci
  expect_equal(m3$residuals, m1$residuals)
  expect_equal(m3$fitted.values, m1$fitted.values)
  expect_equal(
    predict(m1, newdata = mtcars, type = "response"),
    predict(m3, newdata = mtcars, type = "response")
  )
})
