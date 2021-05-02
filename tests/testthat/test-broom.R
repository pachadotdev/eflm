# LM ----

# patrick::with_parameters_test_that("elm + broom is the same as lm:", {
#   m1 <- lm(mpg ~ wt, family = gaussian, data = mtcars)
#   m2 <- elm(mpg ~ wt, family = gaussian, data = mtcars, reduce = reduce)
#
#   tm1 <- broom::tidy(m1)
#   tm2 <- broom::tidy(m2)
#
#   am1 <- broom::augment(m1, newdata = mtcars)
#   am2 <- broom::augment(m2, newdata = mtcars)
#
#   expect_equal(tm2, tm1)
#   expect_equal(am2, am1)
# },
# .cases = elm_cases()
# )

# GLM ----

patrick::with_parameters_test_that("eglm + broom is the same as glm:", {
  m1 <- glm(mpg ~ wt, family = gaussian, data = mtcars)
  m2 <- eglm(mpg ~ wt, family = gaussian, data = mtcars, reduce = reduce)

  tm1 <- broom::tidy(m1)
  tm2 <- broom::tidy(m2)

  # am1 <- broom::augment(m1, newdata = mtcars)
  # am2 <- broom::augment(m2, newdata = mtcars)

  expect_equal(tm2, tm1)
  # expect_equal(am2, am1)
},
.cases = eglm_cases()
)
