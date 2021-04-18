test_that("eglm returns the same broom output as glm", {
  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- elm(mpg ~ wt, data = mtcars, reduce = F)
  m3 <- elm(mpg ~ wt, data = mtcars, reduce = T)

  bm1 <- broom::tidy(m1)
  bm2 <- broom::tidy(m2)
  bm3 <- broom::tidy(m3)

  expect_equal(bm2$term, bm1$term)
  expect_equal(bm2$estimate, bm1$estimate)
  expect_equal(bm3$term, bm1$term)
  expect_equal(bm3$estimate, bm1$estimate)

  expect_equal(round(bm2$std.error, 4), round(bm1$std.error, 4))
  expect_equal(round(bm2$statistic, 4), round(bm1$statistic, 4))

  expect_equal(round(bm3$std.error, 4), round(bm1$std.error, 4))
  expect_equal(round(bm3$statistic, 4), round(bm1$statistic, 4))

  expect_equal(bm2$p.value, bm1$p.value)
  expect_equal(bm3$p.value, bm1$p.value)

  bm1 <- broom::tidy(m1, conf.int = TRUE)
  bm2 <- broom::tidy(m2, conf.int = TRUE)
  # bm3 <- broom::tidy(m3, conf.int = TRUE)

  expect_equal(bm2$conf.low, bm1$conf.low)
  expect_equal(bm2$conf.high, bm1$conf.high)
  # expect_equal(bm3$conf.low, bm1$conf.low)
  # expect_equal(bm3$conf.high, bm1$conf.high)
})

test_that("broom outputs not explicitly defined are the same as glm", {
  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- elm(mpg ~ wt, data = mtcars, reduce = F)
  m3 <- elm(mpg ~ wt, data = mtcars, reduce = T)

  expect_equal(broom::augment(m2, newdata = mtcars), broom::augment(m1, newdata = mtcars))
  expect_equal(broom::augment(m3, newdata = mtcars), broom::augment(m1, newdata = mtcars))
})
