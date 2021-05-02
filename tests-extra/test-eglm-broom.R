test_that("eglm returns the same broom output as glm", {
  m1 <- glm(mpg ~ wt, family = gaussian, data = mtcars)
  m2 <- eflm::eglm(mpg ~ wt, family = gaussian, data = mtcars, reduce = F)
  m3 <- eflm::eglm(mpg ~ wt, family = gaussian, data = mtcars, reduce = T)

  bm1 <- broom::tidy(m1)
  bm2 <- broom::tidy(m2)
  bm3 <- broom::tidy(m3)

  expect_equal(bm2$term, bm1$term)
  expect_equal(bm2$estimate, bm1$estimate)
  expect_equal(bm2$std.error, bm1$std.error)
  expect_equal(bm2$statistic, bm1$statistic)
  expect_equal(bm2$p.value, bm1$p.value)

  bm1 <- broom::tidy(m1, conf.int = TRUE)
  bm2 <- broom::tidy(m2, conf.int = TRUE)
  bm3 <- broom::tidy(m2, conf.int = TRUE)

  expect_equal(bm2$conf.low, bm1$conf.low)
  expect_equal(bm2$conf.high, bm1$conf.high)
  expect_equal(bm3$conf.low, bm1$conf.low)
  expect_equal(bm3$conf.high, bm1$conf.high)

  bm1 <- broom::tidy(m1, exponentiate = TRUE)
  bm2 <- broom::tidy(m2, exponentiate = TRUE)
  bm3 <- broom::tidy(m3, exponentiate = TRUE)

  expect_equal(bm2$term, bm1$term)
  expect_equal(bm3$term, bm1$term)

  expect_equal(log(bm2$estimate), log(bm1$estimate))
  expect_equal(bm2$std.error, bm1$std.error)
  expect_equal(bm2$statistic, bm1$statistic)
  expect_equal(bm2$p.value, bm1$p.value)

  expect_equal(log(bm3$estimate), log(bm1$estimate))
  expect_equal(bm3$std.error, bm1$std.error)
  expect_equal(bm3$statistic, bm1$statistic)
  expect_equal(bm3$p.value, bm1$p.value)
})

test_that("broom outputs not explicitly defined are the same as glm", {
  m1 <- glm(mpg ~ wt, family = gaussian, data = mtcars)
  m2 <- eflm::eglm(mpg ~ wt, family = gaussian, data = mtcars, reduce = F)
  m3 <- eflm::eglm(mpg ~ wt, family = gaussian, data = mtcars, reduce = T)

  expect_equal(broom::augment(m2, newdata = mtcars), broom::augment(m1, newdata = mtcars))
  expect_equal(broom::augment(m3, newdata = mtcars), broom::augment(m1, newdata = mtcars))
})
