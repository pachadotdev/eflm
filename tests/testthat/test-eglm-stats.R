test_that("functions from stats package (except summary) return the same as glm", {
  m1 <- glm(mpg ~ wt, family = gaussian, data = mtcars)
  m2 <- eglm(mpg ~ wt, family = gaussian, data = mtcars)

  fm1 <- family(m1)
  fm2 <- family(m2)

  expect_equal(fm2$family, fm1$family)
  expect_equal(fm2$link, fm1$link)
  expect_equal(fitted(m2), fitted(m1))
  expect_equal(coef(m2), coef(m1))
  expect_equal(vcov(m2), vcov(m1))
  expect_equal(deviance(m2), deviance(m1))
  expect_equal(nobs(m2), nobs(m1))
  expect_equal(model.matrix(m2), model.matrix(m1))
})
