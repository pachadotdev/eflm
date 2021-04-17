test_that("functions from stats package (except summary) return the same as glm", {
  m1 <- glm(mpg ~ wt, family = gaussian, data = mtcars)
  m2 <- eglm(mpg ~ wt, family = gaussian, data = mtcars)

  fm1 <- family(m1)
  fm2 <- family(m2)

  expect_equal(fm1$family, fm2$family)
  expect_equal(fm1$link, fm2$link)
  expect_equal(fitted(m1), fitted(m2))
  expect_equal(coef(m1), coef(m2))
  expect_equal(vcov(m1), vcov(m2))
  expect_equal(deviance(m1), deviance(m2))
  expect_equal(nobs(m1), nobs(m2))
  expect_equal(model.matrix(m1), model.matrix(m2))
})
