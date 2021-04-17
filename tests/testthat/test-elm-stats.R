test_that("functions from stats package (except summary) return the same as lm", {
  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- glm(mpg ~ wt, data = mtcars)

  expect_equal(fitted(m1), fitted(m2))
  expect_equal(coef(m1), coef(m2))
  expect_equal(vcov(m1), vcov(m2))
  expect_equal(deviance(m1), deviance(m2))
  expect_equal(nobs(m1), nobs(m2))
  expect_equal(model.matrix(m1), model.matrix(m2))
})
