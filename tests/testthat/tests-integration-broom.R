test_that("functions in R/integration-broom.R return the same as glm", {
  m1 <- glm(mpg ~ wt, family = gaussian(), data = mtcars)
  m2 <- fglm(mpg ~ wt, family = gaussian(), data = mtcars)

  bm1 <- broom::tidy(m1)
  bm2 <- broom::tidy(m2)

  expect_equal(bm1$term, bm2$term)
  expect_equal(bm1$estimate, bm2$estimate)
  expect_equal(round(bm1$std.error, 4), round(bm2$std.error, 4))
  expect_equal(round(bm1$statistic, 4), round(bm2$statistic, 4))
  expect_equal(bm1$p.value, bm2$p.value)

  bm1 <- broom::tidy(m1, conf.int = TRUE)
  bm2 <- broom::tidy(m2, conf.int = TRUE)

  expect_equal(bm1$conf.low, bm2$conf.low)
  expect_equal(bm1$conf.high, bm2$conf.high)

  bm1 <- broom::tidy(m1, exponentiate = TRUE)
  bm2 <- broom::tidy(m2, exponentiate = TRUE)

  expect_equal(bm1$term, bm2$term)
  # broom exponentiation gives a rounding problem
  expect_equal(round(log(bm1$estimate), 4), round(log(bm2$estimate), 4))
  expect_equal(round(bm1$std.error, 4), round(bm2$std.error, 4))
  expect_equal(round(bm1$statistic, 4), round(bm2$statistic, 4))
  expect_equal(bm1$p.value, bm2$p.value)
})

test_that("broom outputs not explicitly defined are the same as glm", {
  m1 <- glm(mpg ~ wt, family = gaussian(), data = mtcars)
  m2 <- fglm(mpg ~ wt, family = gaussian(), data = mtcars)

  broom:::augment.glm()
  expect_equal(broom::augment(m1), broom::augment(m2))
})
