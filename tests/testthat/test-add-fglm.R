test_that("gaussian add is equivalent to glm", {
  m1 <- glm(Fertility ~ ., data = swiss, family = gaussian())
  add_m1 <- add1(m1, ~ I(Education^2) + .^2)

  m2 <- fglm(Fertility ~ ., data = swiss, family = gaussian(), model = TRUE)
  add_m2 <- add1(m2, ~ I(Education^2) + .^2)

  # FALTA EL AIC EN M2
})
