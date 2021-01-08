test_that("gaussian add is equivalent to glm", {
  m1 <- glm(Fertility ~ ., data = swiss, family = gaussian())
  add_m1 <- add1(m1, ~ I(Education^2) + .^2)

  m2 <- fglm(Fertility ~ ., data = swiss, family = gaussian(), model = TRUE)
  add_m2 <- add1(m2, ~ I(Education^2) + .^2)

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
})
