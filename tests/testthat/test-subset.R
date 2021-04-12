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

  # FIX
  # expect_equal(m1$residuals, m2$residuals)
  # expect_equal(m1$fitted.values, m2$fitted.values)
})

# test_that("eglm (gaussian) == glm", {
#   m1 <- glm(log(mpg) ~ log(wt), data = mtcars, subset = (cyl == 4 & am == 1))
#   m2 <- eglm(log(mpg) ~ log(wt), data = mtcars, subset = (cyl == 4 & am == 1))
#
#   expect_equal(m1$coefficients, m2$coefficients)
#   expect_equal(m1$residuals, m2$residuals)
#   expect_equal(m1$fitted.values, m2$fitted.values)
#   expect_equal(m1$family$family, m2$family$family)
#   expect_equal(m1$family$link, m2$family$link)
#   expect_equal(m1$linear.predictors, m2$linear.predictors)
#   expect_equal(m1$deviance, m2$deviance)
#   expect_equal(m1$aic, m2$aic)
#   expect_equal(m1$null.deviance, m2$null.deviance)
#   expect_equal(m1$df.residual, m2$df.residual)
#   expect_equal(m1$df.null, m2$df.null)
#   expect_equal(m1$y, m2$y)
#   expect_equal(m1$call$formula, m2$call$formula)
#   expect_equal(m1$call$family, m2$call$family)
#   expect_equal(m1$call$data, m2$call$data)
#   expect_equal(m1$qr$tol, m2$qr$tol)
#
#   expect_equal(m1$qr$qr, m2$qr$qr)
#   expect_equal(m1$qr$rank, m2$qr$rank)
#   expect_equal(m1$qr$qraux, m2$qr$qraux)
#   expect_equal(m1$qr$pivot, m2$qr$pivot)
# })
