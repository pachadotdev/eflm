# Gaussian ----

test_that("eglm (gaussian) == glm", {
  m1 <- glm(mpg ~ wt + am, family = gaussian, data = mtcars)
  m2 <- eglm(mpg ~ wt + am, family = gaussian, data = mtcars, reduce = F)
  m3 <- eglm(mpg ~ wt + am, family = gaussian, data = mtcars, reduce = T)

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
  expect_equal(m2$qr$tol, m1$qr$tol)

  expect_equal(m2$qr$qr, m1$qr$qr)
  expect_equal(m2$qr$rank, m1$qr$rank)
  expect_equal(m2$qr$qraux, m1$qr$qraux)
  expect_equal(m2$qr$pivot, m1$qr$pivot)

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
  expect_equal(m3$qr$tol, m1$qr$tol)

  # the QR decomposition is different by definition
  # expect_equal(m3$qr$qr, m1$qr$qr)
  # expect_equal(m3$qr$qraux, m1$qr$qraux)
  expect_equal(m3$qr$rank, m1$qr$rank)
  expect_equal(m3$qr$pivot, m1$qr$pivot)
  expect_lte(prod(dim(m3)), prod(dim(m1)))

  expect_equal(
    predict(m2, newdata = mtcars, type = "link"),
    predict(m1, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m2, newdata = mtcars, type = "response"),
    predict(m1, newdata = mtcars, type = "response")
  )

  expect_equal(
    predict(m3, newdata = mtcars, type = "link"),
    predict(m1, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m3, newdata = mtcars, type = "response"),
    predict(m1, newdata = mtcars, type = "response")
  )
})

# Inverse-Gaussian ----

test_that("eglm (inverse.gaussian) == glm", {
  m1 <- glm(mpg ~ wt + am, family = inverse.gaussian, data = mtcars)
  m2 <- eglm(mpg ~ wt + am, family = inverse.gaussian, data = mtcars, reduce = F)
  m3 <- eglm(mpg ~ wt + am, family = inverse.gaussian, data = mtcars, reduce = T)

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

  expect_equal(
    predict(m2, newdata = mtcars, type = "link"),
    predict(m1, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m2, newdata = mtcars, type = "response"),
    predict(m1, newdata = mtcars, type = "response")
  )

  expect_equal(
    predict(m3, newdata = mtcars, type = "link"),
    predict(m1, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m3, newdata = mtcars, type = "response"),
    predict(m1, newdata = mtcars, type = "response")
  )
})

# Gamma ----

test_that("eglm (gamma) == glm", {
  m1 <- glm(mpg ~ wt + am, family = Gamma, data = mtcars)
  m2 <- eglm(mpg ~ wt + am, family = Gamma, data = mtcars, reduce = F)
  m3 <- eglm(mpg ~ wt + am, family = Gamma, data = mtcars, reduce = T)

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

  expect_equal(
    predict(m2, newdata = mtcars, type = "link"),
    predict(m1, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m2, newdata = mtcars, type = "response"),
    predict(m1, newdata = mtcars, type = "response")
  )

  expect_equal(
    predict(m3, newdata = mtcars, type = "link"),
    predict(m1, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m3, newdata = mtcars, type = "response"),
    predict(m1, newdata = mtcars, type = "response")
  )
})

# Binomial ----

test_that("eglm (binomial) == glm", {
  m1 <- glm(am ~ wt + mpg, family = binomial, data = mtcars)
  m2 <- eglm(am ~ wt + mpg, family = binomial, data = mtcars, reduce = F)
  m3 <- eglm(am ~ wt + mpg, family = binomial, data = mtcars, reduce = T)

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

  expect_equal(
    predict(m2, newdata = mtcars, type = "link"),
    predict(m1, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m2, newdata = mtcars, type = "response"),
    predict(m1, newdata = mtcars, type = "response")
  )

  expect_equal(
    predict(m3, newdata = mtcars, type = "link"),
    predict(m1, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m3, newdata = mtcars, type = "response"),
    predict(m1, newdata = mtcars, type = "response")
  )
})

# Quasi-Binomial ----

test_that("eglm (quasibinomial) == glm", {
  m1 <- glm(am ~ wt + mpg, family = quasibinomial, data = mtcars)
  m2 <- eglm(am ~ wt + mpg, family = quasibinomial, data = mtcars, reduce = F)
  m3 <- eglm(am ~ wt + mpg, family = quasibinomial, data = mtcars, reduce = T)

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

  expect_equal(
    predict(m2, newdata = mtcars, type = "link"),
    predict(m1, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m2, newdata = mtcars, type = "response"),
    predict(m1, newdata = mtcars, type = "response")
  )

  expect_equal(
    predict(m3, newdata = mtcars, type = "link"),
    predict(m1, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m3, newdata = mtcars, type = "response"),
    predict(m1, newdata = mtcars, type = "response")
  )
})

# Poisson ----

test_that("eglm (poisson) == glm", {
  m1 <- glm(am ~ wt + mpg, family = poisson, data = mtcars)
  m2 <- eglm(am ~ wt + mpg, family = poisson, data = mtcars, reduce = F)
  m3 <- eglm(am ~ wt + mpg, family = poisson, data = mtcars, reduce = T)

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

  expect_equal(
    predict(m2, newdata = mtcars, type = "link"),
    predict(m1, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m2, newdata = mtcars, type = "response"),
    predict(m1, newdata = mtcars, type = "response")
  )

  expect_equal(
    predict(m3, newdata = mtcars, type = "link"),
    predict(m1, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m3, newdata = mtcars, type = "response"),
    predict(m1, newdata = mtcars, type = "response")
  )
})

# Quasi-Poisson ----

test_that("eglm (quasipoisson) == glm", {
  m1 <- glm(mpg ~ wt + am, family = quasipoisson, data = mtcars)
  m2 <- eglm(mpg ~ wt + am, family = quasipoisson, data = mtcars, reduce = F)
  m3 <- eglm(mpg ~ wt + am, family = quasipoisson, data = mtcars, reduce = T)

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

  expect_equal(
    predict(m2, newdata = mtcars, type = "link"),
    predict(m1, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m2, newdata = mtcars, type = "response"),
    predict(m1, newdata = mtcars, type = "response")
  )

  expect_equal(
    predict(m3, newdata = mtcars, type = "link"),
    predict(m1, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m3, newdata = mtcars, type = "response"),
    predict(m1, newdata = mtcars, type = "response")
  )
})

# Quasi ----

test_that("eglm (quasi) == glm", {
  m1 <- glm(mpg ~ wt + am, family = quasi, data = mtcars)
  m2 <- eglm(mpg ~ wt + am, family = quasi, data = mtcars, reduce = F)
  m3 <- eglm(mpg ~ wt + am, family = quasi, data = mtcars, reduce = T)

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

  expect_equal(
    predict(m2, newdata = mtcars, type = "link"),
    predict(m1, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m2, newdata = mtcars, type = "response"),
    predict(m1, newdata = mtcars, type = "response")
  )

  expect_equal(
    predict(m3, newdata = mtcars, type = "link"),
    predict(m1, newdata = mtcars, type = "link")
  )
  expect_equal(
    predict(m3, newdata = mtcars, type = "response"),
    predict(m1, newdata = mtcars, type = "response")
  )
})

# Convergence ----

test_that("eglm converges the same as glm with a regular model", {
  m1 <- glm(am ~ mpg + wt, family = binomial, data = mtcars)
  m2 <- eglm(am ~ mpg + wt, family = binomial, data = mtcars, reduce = F)
  m3 <- eglm(am ~ mpg + wt, family = binomial, data = mtcars, reduce = T)

  expect_equal(m2$iter, m1$iter)
  expect_equal(m2$convergence, m1$convergence)

  expect_equal(m3$iter, m1$iter)
  expect_equal(m3$convergence, m1$convergence)
})

test_that("eglm logit fails to converge with a large number of variables", {
  # Warning messages:
  #  1: glm.fit: algorithm did not converge
  #  2: glm.fit: fitted probabilities numerically 0 or 1 occurred
  expect_warning(
    expect_warning(glm(am ~ ., family = binomial, data = mtcars))
  )

  # Warning messages:
  #  1: eglm.wfit: algorithm did not converge
  #  2: eglm.wfit: fitted probabilities numerically 0 or 1 occurred
  expect_warning(
    expect_warning(eglm(am ~ ., family = binomial, data = mtcars, reduce = F))
  )

  expect_warning(
    expect_warning(eglm(am ~ ., family = binomial, data = mtcars, reduce = T))
  )
})

# Fitting ----

test_that("eglm returns the same fitted values as glm", {
  m1 <- glm(am ~ mpg + wt, family = binomial, data = mtcars)
  m2 <- eglm(am ~ mpg + wt, family = binomial, data = mtcars, reduce = F)
  m3 <- eglm(am ~ mpg + wt, family = binomial, data = mtcars, reduce = T)

  expect_equal(m2$fitted.values, m1$fitted.values)
  expect_equal(fitted(m2), fitted(m1))

  expect_equal(m3$fitted.values, m1$fitted.values)
  expect_equal(fitted(m3), fitted(m1))
})

# Design matrix ----

test_that("eglm returns the design matrix the same as glm", {
  m1 <- glm(mpg ~ wt, data = mtcars, x = TRUE)
  m2 <- eglm(mpg ~ wt, data = mtcars, x = TRUE, reduce = FALSE)
  m3 <- eglm(mpg ~ wt, data = mtcars, x = TRUE, reduce = TRUE)

  expect_equal(m2$x, m1$x)
  expect_equal(m3$x, m1$x)
})
