test_that("gaussian drop is equivalent to glm", {
  m1 <- glm(Fertility ~ ., data = swiss, family = gaussian())
  drop_m1 <- drop1(m1, test = "F")

  m2 <- fglm(Fertility ~ ., data = swiss, family = gaussian())
  drop_m2 <- drop1(m2, test = "F")
})
