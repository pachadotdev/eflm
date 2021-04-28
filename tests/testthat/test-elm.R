# Fitting ----

test_that(sprintf("fitting: elm == lm, unweighted"), {
  m <- "am ~ wt + mpg"

  m1 <- lm(m, data = mtcars)
  m2 <- elm(m, data = mtcars, reduce = F)
  m3 <- elm(m, data = mtcars, reduce = T)

  expect_model_equal(m2,m1)
  expect_model_equal(m3,m1)
})

test_that(sprintf("fitting: elm == lm, weighted"), {
  m <- "am ~ wt + mpg"

  m1 <- lm(m, data = mtcars, weights = mtcars$cyl)
  m2 <- elm(m, data = mtcars, weights = mtcars$cyl, reduce = F)
  m3 <- elm(m, data = mtcars, weights = mtcars$cyl, reduce = T)

  expect_model_equal(m2,m1)
  expect_model_equal(m3,m1)
})

# Design matrix ----

test_that("design matrix: elm == lm", {
  m1 <- lm(mpg ~ wt, data = mtcars, x = TRUE)
  m2 <- elm(mpg ~ wt, data = mtcars, x = TRUE, reduce = FALSE)
  m3 <- elm(mpg ~ wt, data = mtcars, x = TRUE, reduce = TRUE)

  expect_equal(m2$x, m1$x)
  expect_equal(m3$x, m1$x)
})
