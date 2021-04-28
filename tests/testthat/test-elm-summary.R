test_that("elm summary is equivalent to lm", {
  m1 <- summary(lm(mpg ~ wt, data = mtcars))
  m2 <- summary(elm(mpg ~ wt, data = mtcars, reduce = F))
  m3 <- summary(elm(mpg ~ wt, data = mtcars, reduce = T))

  expect_summary_equal(m2, m1)
  expect_summary_equal(m3, m1)
})
