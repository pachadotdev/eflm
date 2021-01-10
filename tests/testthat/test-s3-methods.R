# Summary ----

test_that("summary.fglm fails with non-fglm objects", {
  expect_error(hermes:::summary.fglm(list()))
})

# Predict ----

test_that("predict.fglm returns warning with NULL linear.predictors and missing newdata", {
  m1 <- fglm(mpg ~ wt, family = gaussian(), data = mtcars)
  m1$linear.predictors <- NULL
  expect_warning(hermes:::predict.fglm(m1))
})

# test_that("predict.fglm accepts na.action", {
#   m1 <- fglm(mpg ~ wt, family = gaussian(), data = mtcars)
#   hermes:::predict.fglm(m1, na.action = na.omit)
# })
