# LM ----

patrick::with_parameters_test_that("elm + broom is the same as lm:", {
  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- elm(mpg ~ wt, data = mtcars, reduce = reduce)

  tm1 <- broom::tidy(m1)
  tm2 <- broom::tidy(m2)

  am1 <- broom::augment(m1, newdata = mtcars)
  am2 <- broom::augment(m2, newdata = mtcars)

  expect_equal(tm2, tm1)
  expect_equal(am2, am1)
},
.cases = elm_cases()
)

# test_that("elm + tidy + conf.int / exponentiate is the same as lm", {
#   m1 <- glm(mpg ~ wt, family = gaussian, data = mtcars)
#   m2 <- eflm::elm(mpg ~ wt, data = mtcars, reduce = F)
#   m3 <- eflm::elm(mpg ~ wt, data = mtcars, reduce = T)
#
#   bm1 <- broom::tidy(m1, conf.int = TRUE, conf.level = 0.9)
#   bm2 <- broom::tidy(m2, conf.int = TRUE, conf.level = 0.9)
#   bm3 <- broom::tidy(m3, conf.int = TRUE, conf.level = 0.9)
#
#   expect_equal(bm2, bm1)
#   expect_equal(bm3, bm1)
# })

# GLM ----

patrick::with_parameters_test_that("eglm + broom is the same as glm:", {
  m1 <- glm(mpg ~ wt, family = gaussian, data = mtcars)
  m2 <- eglm(mpg ~ wt, family = gaussian, data = mtcars, reduce = reduce)

  tm1 <- broom::tidy(m1)
  tm2 <- broom::tidy(m2)

  am1 <- broom::augment(m1, newdata = mtcars)
  am2 <- broom::augment(m2, newdata = mtcars)

  expect_equal(tm2, tm1)
  expect_equal(am2, am1)
},
.cases = eglm_cases()
)

# test_that("eglm + tidy + conf.int / exponentiate is the same as glm", {
#   m1 <- glm(mpg ~ wt, family = gaussian, data = mtcars)
#   m2 <- eflm::eglm(mpg ~ wt, family = gaussian, data = mtcars, reduce = F)
#   m3 <- eflm::eglm(mpg ~ wt, family = gaussian, data = mtcars, reduce = T)
#
#   bm1 <- broom::tidy(m1, conf.int = TRUE, conf.level = 0.9)
#   bm2 <- broom::tidy(m2, conf.int = TRUE, conf.level = 0.9)
#   bm3 <- broom::tidy(m3, conf.int = TRUE, conf.level = 0.9)
#
#   expect_equal(bm2, bm1)
#   expect_equal(bm3, bm1)
#
#   bm1 <- broom::tidy(m1, exponentiate = TRUE)
#   bm2 <- broom::tidy(m2, exponentiate = TRUE)
#   bm3 <- broom::tidy(m3, exponentiate = TRUE)
#
#   expect_equal(bm2, bm1)
#   expect_equal(bm3, bm1)
# })
