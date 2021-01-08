# Gaussian ----

test_that("fglm (gaussian) == glm with 1 variable", {
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
  expect_equal(m1$y, m2$y)
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

test_that("fglm (gaussian) == glm with 2 variables", {
  m1 <- glm(mpg ~ wt + am, family = gaussian(), data = mtcars)
  m2 <- fglm(mpg ~ wt + am, family = gaussian(), data = mtcars)

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

  expect_equal(
    predict(m1, newdata = mtcars, type = "link"),
    predict(m2, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m1, newdata = mtcars, type = "response"),
    predict(m2, newdata = mtcars, type = "response")
  )
})

# Inverse-Gaussian ----

test_that("fglm (inverse.gaussian) == glm with 1 variable", {
  m1 <- glm(mpg ~ wt, family = inverse.gaussian(), data = mtcars)
  m2 <- fglm(mpg ~ wt, family = inverse.gaussian(), data = mtcars)

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

  expect_equal(
    predict(m1, newdata = mtcars, type = "link"),
    predict(m2, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m1, newdata = mtcars, type = "response"),
    predict(m2, newdata = mtcars, type = "response")
  )
})

test_that("fglm (inverse.gaussian) == glm with 2 variables", {
  m1 <- glm(mpg ~ wt + am, family = inverse.gaussian(), data = mtcars)
  m2 <- fglm(mpg ~ wt + am, family = inverse.gaussian(), data = mtcars)

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

  expect_equal(
    predict(m1, newdata = mtcars, type = "link"),
    predict(m2, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m1, newdata = mtcars, type = "response"),
    predict(m2, newdata = mtcars, type = "response")
  )
})

# Binomial ----

test_that("fglm (binomial) == glm with 1 variable", {
  m1 <- glm(am ~ wt, family = binomial(), data = mtcars)
  m2 <- fglm(am ~ wt, family = binomial(), data = mtcars)

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

  expect_equal(
    predict(m1, newdata = mtcars, type = "link"),
    predict(m2, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m1, newdata = mtcars, type = "response"),
    predict(m2, newdata = mtcars, type = "response")
  )
})

test_that("fglm (binomial) == glm with 2 variables", {
  m1 <- glm(am ~ wt + mpg, family = binomial(), data = mtcars)
  m2 <- fglm(am ~ wt + mpg, family = binomial(), data = mtcars)

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

  expect_equal(
    predict(m1, newdata = mtcars, type = "link"),
    predict(m2, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m1, newdata = mtcars, type = "response"),
    predict(m2, newdata = mtcars, type = "response")
  )
})

# Quasi-Binomial ----

test_that("fglm (quasibinomial) == glm with 1 variable", {
  m1 <- glm(am ~ wt, family = quasibinomial(), data = mtcars)
  m2 <- fglm(am ~ wt, family = quasibinomial(), data = mtcars)

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

  expect_equal(
    predict(m1, newdata = mtcars, type = "link"),
    predict(m2, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m1, newdata = mtcars, type = "response"),
    predict(m2, newdata = mtcars, type = "response")
  )
})

test_that("fglm (quasibinomial) == glm with 2 variables", {
  m1 <- glm(am ~ wt + mpg, family = quasibinomial(), data = mtcars)
  m2 <- fglm(am ~ wt + mpg, family = quasibinomial(), data = mtcars)

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

  expect_equal(
    predict(m1, newdata = mtcars, type = "link"),
    predict(m2, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m1, newdata = mtcars, type = "response"),
    predict(m2, newdata = mtcars, type = "response")
  )
})

# Poisson ----

test_that("fglm (poisson) == glm with 1 variable", {
  m1 <- glm(am ~ wt, family = poisson(), data = mtcars)
  m2 <- fglm(am ~ wt, family = poisson(), data = mtcars)

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

  expect_equal(
    predict(m1, newdata = mtcars, type = "link"),
    predict(m2, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m1, newdata = mtcars, type = "response"),
    predict(m2, newdata = mtcars, type = "response")
  )
})

test_that("fglm (poisson) == glm with 2 variables", {
  m1 <- glm(am ~ wt + mpg, family = poisson(), data = mtcars)
  m2 <- fglm(am ~ wt + mpg, family = poisson(), data = mtcars)

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

  expect_equal(
    predict(m1, newdata = mtcars, type = "link"),
    predict(m2, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m1, newdata = mtcars, type = "response"),
    predict(m2, newdata = mtcars, type = "response")
  )
})

# Quasi-Poisson ----

test_that("fglm (quasipoisson) == glm with 1 variable", {
  m1 <- glm(mpg ~ wt, family = quasipoisson(), data = mtcars)
  m2 <- fglm(mpg ~ wt, family = quasipoisson(), data = mtcars)

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

  expect_equal(
    predict(m1, newdata = mtcars, type = "link"),
    predict(m2, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m1, newdata = mtcars, type = "response"),
    predict(m2, newdata = mtcars, type = "response")
  )
})

test_that("fglm (quasipoisson) == glm with 2 variables", {
  m1 <- glm(mpg ~ wt + am, family = quasipoisson(), data = mtcars)
  m2 <- fglm(mpg ~ wt + am, family = quasipoisson(), data = mtcars)

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

  expect_equal(
    predict(m1, newdata = mtcars, type = "link"),
    predict(m2, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m1, newdata = mtcars, type = "response"),
    predict(m2, newdata = mtcars, type = "response")
  )
})

# Quasi ----

test_that("fglm (quasi) == glm with 1 variable", {
  m1 <- glm(mpg ~ wt, family = quasi(), data = mtcars)
  m2 <- fglm(mpg ~ wt, family = quasi(), data = mtcars)

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

  expect_equal(
    predict(m1, newdata = mtcars, type = "link"),
    predict(m2, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m1, newdata = mtcars, type = "response"),
    predict(m2, newdata = mtcars, type = "response")
  )
})

test_that("fglm (quasi) == glm with 2 variables", {
  m1 <- glm(mpg ~ wt + am, family = quasi(), data = mtcars)
  m2 <- fglm(mpg ~ wt + am, family = quasi(), data = mtcars)

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

  expect_equal(
    predict(m1, newdata = mtcars, type = "link"),
    predict(m2, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m1, newdata = mtcars, type = "response"),
    predict(m2, newdata = mtcars, type = "response")
  )
})
