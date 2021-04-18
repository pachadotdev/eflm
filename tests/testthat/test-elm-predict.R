# Gaussian ----

test_that("eglm (gaussian) == glm", {
  m1 <- lm(mpg ~ wt + am, data = mtcars)
  m2 <- elm(mpg ~ wt + am, data = mtcars, reduce = F)
  m3 <- elm(mpg ~ wt + am, data = mtcars, reduce = T)

  expect_equal(
    predict(m2, newdata = mtcars),
    predict(m1, newdata = mtcars)
  )

  expect_equal(
    predict(m3, newdata = mtcars),
    predict(m1, newdata = mtcars)
  )
})
