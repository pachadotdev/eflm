test_that("functions from stats package (except summary) return the same as lm", {
  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- elm(mpg ~ wt, data = mtcars, reduce = F)
  m3 <- elm(mpg ~ wt, data = mtcars, reduce = T)

  expect_equal(fitted(m2), fitted(m1))
  expect_equal(coef(m2), coef(m1))
  expect_equal(vcov(m2), vcov(m1))
  expect_equal(deviance(m2), deviance(m1))
  expect_equal(nobs(m2), nobs(m1))
  expect_equal(model.matrix(m2), model.matrix(m1))

  expect_equal(fitted(m3), fitted(m1))
  expect_equal(coef(m3), coef(m1))
  expect_equal(vcov(m3), vcov(m1))
  expect_equal(deviance(m3), deviance(m1))
  expect_equal(nobs(m3), nobs(m1))
  expect_equal(model.matrix(m3), model.matrix(m1))
})
