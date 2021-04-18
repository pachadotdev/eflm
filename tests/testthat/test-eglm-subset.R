test_that("subset works and returns the same results for glm and eglm", {
  m1 <- glm(mpg ~ wt + am, family = gaussian, data = mtcars, subset = (cyl == 4 & am == 1))
  m2 <- eglm(mpg ~ wt + am, family = gaussian, data = mtcars, subset = (cyl == 4 & am == 1), reduce = F)
  m3 <- eglm(mpg ~ wt + am, family = gaussian, data = mtcars, subset = (cyl == 4 & am == 1), reduce = T)

  expect_equal(m2$coefficients, m1$coefficients)
  expect_equal(m2$residuals, m1$residuals)
  expect_equal(m2$fitted.values, m1$fitted.values)
  expect_equal(m2$family$family, m1$family$family)
  expect_equal(m2$family$link, m1$family$link)
  expect_equal(m2$linear.predictors, m1$linear.predictors)
  expect_equal(m2$deviance, m1$deviance)
  expect_equal(m2$aic, m1$aic)
  expect_equal(m2$null.deviance, m1$null.deviance)
  expect_equal(m2$df.residual, m1$df.residual)
  expect_equal(m2$df.null, m1$df.null)
  expect_equal(m2$y, m1$y)
  expect_equal(m2$call$formula, m1$call$formula)
  expect_equal(m2$call$family, m1$call$family)
  expect_equal(m2$call$data, m1$call$data)

  expect_equal(m2$residuals, m1$residuals)
  expect_equal(m2$fitted.values, m1$fitted.values)

  # prediction from a rank-deficient fit here !!
  expect_equal(
    suppressWarnings(predict(m2, newdata = mtcars, type = "link")),
    suppressWarnings(predict(m1, newdata = mtcars, type = "link"))
  )
  expect_equal(
    suppressWarnings(predict(m2, newdata = mtcars, type = "response")),
    suppressWarnings(predict(m1, newdata = mtcars, type = "response"))
  )

  expect_equal(m3$coefficients, m1$coefficients)
  expect_equal(m3$residuals, m1$residuals)
  expect_equal(m3$fitted.values, m1$fitted.values)
  expect_equal(m3$family$family, m1$family$family)
  expect_equal(m3$family$link, m1$family$link)
  expect_equal(m3$linear.predictors, m1$linear.predictors)
  expect_equal(m3$deviance, m1$deviance)
  expect_equal(m3$aic, m1$aic)
  expect_equal(m3$null.deviance, m1$null.deviance)
  expect_equal(m3$df.residual, m1$df.residual)
  expect_equal(m3$df.null, m1$df.null)
  expect_equal(m3$y, m1$y)
  expect_equal(m3$call$formula, m1$call$formula)
  expect_equal(m3$call$family, m1$call$family)
  expect_equal(m3$call$data, m1$call$data)

  expect_equal(m3$residuals, m1$residuals)
  expect_equal(m3$fitted.values, m1$fitted.values)

  # prediction from a rank-deficient fit here !!
  expect_equal(
    suppressWarnings(predict(m3, newdata = mtcars, type = "link")),
    suppressWarnings(predict(m1, newdata = mtcars, type = "link"))
  )
  expect_equal(
    suppressWarnings(predict(m3, newdata = mtcars, type = "response")),
    suppressWarnings(predict(m1, newdata = mtcars, type = "response"))
  )
})
