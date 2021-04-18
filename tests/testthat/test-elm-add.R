# Gaussian ----

test_that("Add: eglm (gaussian) == glm with no test", {
  m1 <- lm(mpg ~ wt, data = mtcars)
  add_m1 <- add1(m1, ~ . + am)

  m2 <- elm(mpg ~ wt, data = mtcars, reduce = T)
  add_m2 <- add1(m2, ~ . + am)

  m3 <- elm(mpg ~ wt, data = mtcars, reduce = F)
  add_m3 <- add1(m3, ~ . + am)

  expect_equal(add_m2$Df, add_m1$Df)
  expect_equal(add_m2$Deviance, add_m1$Deviance)
  expect_equal(add_m2$AIC, add_m1$AIC)

  expect_equal(add_m2$Df, add_m3$Df)
  expect_equal(add_m2$Deviance, add_m3$Deviance)
  expect_equal(add_m2$AIC, add_m3$AIC)
})
