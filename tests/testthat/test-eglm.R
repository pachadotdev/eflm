# Fitting ----

fmly <- c("gaussian", "inverse.gaussian", "Gamma", "binomial", "quasibinomial",
          "poisson", "quasipoisson", "quasi")

wgts <- list(x = rep(1, nrow(mtcars)), y = mtcars$cyl)

mdl <- function() {
  if (!any(f %in% c("poisson", "binomial", "quasibinomial"))) {
    "mpg ~ wt + am"
  } else {
    "am ~ wt + mpg"
  }
}

mdl_expc <- function(m1,m2,m3) {
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

  expect_equal(m2$iter, m1$iter)
  expect_equal(m2$convergence, m1$convergence)

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

  expect_equal(m3$iter, m1$iter)
  expect_equal(m3$convergence, m1$convergence)

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
  expect_equal(fitted(m2), fitted(m1))
  expect_equal(fitted(m3), fitted(m1))
}

for (f in fmly) {
  test_that(sprintf("fitting: eglm == glm, %s link, unweighted", f), {
    m <- mdl()

    m1 <- glm(m, family = f, data = mtcars)
    m2 <- eglm(m, family = f, data = mtcars, reduce = F)
    m3 <- eglm(m, family = f, data = mtcars, reduce = T)

    mdl_expc(m1,m2,m3)
  })
}

for (f in fmly) {
  test_that(sprintf("fitting: eglm == glm, %s link, weighted", f), {
    m <- mdl()

    m1 <- glm(m, family = f, data = mtcars, weights = mtcars$cyl)
    m2 <- eglm(m, family = f, data = mtcars, weights = mtcars$cyl, reduce = F)
    m3 <- eglm(m, family = f, data = mtcars, weights = mtcars$cyl, reduce = T)

    mdl_expc(m1,m2,m3)
  })
}

# Convergence ----

for (f in fmly) {
  test_that(sprintf("convergence: eglm == glm, %s link", f), {
    m <- mdl()
    m1 <- glm(am ~ mpg + wt, family = binomial, data = mtcars, weights = mtcars$cyl)
    m2 <- eglm(am ~ mpg + wt, family = binomial, data = mtcars, weights = mtcars$cyl, reduce = F)
    m3 <- eglm(am ~ mpg + wt, family = binomial, data = mtcars, weights = mtcars$cyl, reduce = T)

    expect_equal(m2$iter, m1$iter)
    expect_equal(m2$convergence, m1$convergence)

    expect_equal(m3$iter, m1$iter)
    expect_equal(m3$convergence, m1$convergence)
  })
}

test_that("convergence: eglm == glm, logit fails to converge with a large number of variables", {
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

# Design matrix ----

test_that("design matrix: eglm == glm", {
  m1 <- glm(mpg ~ wt, data = mtcars, x = TRUE)
  m2 <- eglm(mpg ~ wt, data = mtcars, x = TRUE, reduce = FALSE)
  m3 <- eglm(mpg ~ wt, data = mtcars, x = TRUE, reduce = TRUE)

  expect_equal(m2$x, m1$x)
  expect_equal(m3$x, m1$x)
})
