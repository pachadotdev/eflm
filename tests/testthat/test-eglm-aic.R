test_that("Same AIC for glm and eglm with gaussian link", {
  m1 <- glm(mpg ~ wt + am, family = gaussian, data = mtcars)
  m2 <- eglm(mpg ~ wt + am, family = gaussian, data = mtcars, reduce = FALSE)
  m3 <- eglm(mpg ~ wt + am, family = gaussian, data = mtcars, reduce = TRUE)

  expect_equal(AIC(m2), AIC(m1))
  expect_equal(AIC(m3), AIC(m1))

  aic_m1_m2_m3 <- AIC(m1, m2, m3)
  expect_s3_class(aic_m1_m2_m3, "data.frame")
  expect_equal(aic_m1_m2_m3$df[2], aic_m1_m2_m3$df[1])
  expect_equal(aic_m1_m2_m3$AIC[2], aic_m1_m2_m3$AIC[1])
  expect_equal(aic_m1_m2_m3$df[3], aic_m1_m2_m3$df[1])
  expect_equal(aic_m1_m2_m3$AIC[3], aic_m1_m2_m3$AIC[1])
})

test_that("Same AIC for glm and eglm with Gamma link", {
  m1 <- glm(mpg ~ wt + am, family = Gamma, data = mtcars)
  m2 <- eglm(mpg ~ wt + am, family = Gamma, data = mtcars, reduce = FALSE)
  m3 <- eglm(mpg ~ wt + am, family = Gamma, data = mtcars, reduce = TRUE)
  expect_equal(AIC(m2), AIC(m1))
  expect_equal(AIC(m3), AIC(m1))
})
