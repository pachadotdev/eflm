test_that("family arguments are correctly converted", {
  # gaussian, gaussian() and "gaussian" are converted to gaussian()
  m1 <- eglm(mpg ~ wt, family = gaussian, data = mtcars)
  m2 <- eglm(mpg ~ wt, family = gaussian(), data = mtcars)
  m3 <- eglm(mpg ~ wt, family = "gaussian", data = mtcars)

  expect_s3_class(m1, "glm")
  expect_s3_class(m2, "glm")
  expect_s3_class(m3, "glm")

  expect_equal(m2$coefficients, m1$coefficients)
  expect_equal(m3$coefficients, m1$coefficients)
  expect_equal(m3$coefficients, m2$coefficients)
})
