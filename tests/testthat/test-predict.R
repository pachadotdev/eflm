# Sanity check ----

test_that("predict.fglm fails with NULL fitted values + newdata", {
  m <- fglm(mpg ~ wt + am, family = gaussian(), data = mtcars)
  m$fitted.values <- NULL
  expect_warning(predict(m, newdata = NULL))
})
