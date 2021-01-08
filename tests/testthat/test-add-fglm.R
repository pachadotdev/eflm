# Gaussian ----

test_that("Add: fglm (gaussian) == glm with no test", {
  m1 <- glm(Fertility ~ ., data = swiss, family = gaussian())
  add_m1 <- add1(m1, ~ I(Education^2) + .^2)

  m2 <- fglm(Fertility ~ ., data = swiss, family = gaussian())
  add_m2 <- add1(m2, ~ I(Education^2) + .^2)

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
})

test_that("Add: fglm (gaussian) == glm with F test", {
  m1 <- glm(Fertility ~ ., data = swiss, family = gaussian())
  add_m1 <- add1(m1, ~ I(Education^2) + .^2, test = "F")

  m2 <- fglm(Fertility ~ ., data = swiss, family = gaussian())
  add_m2 <- add1(m2, ~ I(Education^2) + .^2, test = "F")

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`F value`, add_m2$`F value`)
  expect_equal(add_m1$`Pr(>F)`, add_m2$`Pr(>F)`)
})

test_that("Add: fglm (gaussian) == glm with Chisq test", {
  m1 <- glm(Fertility ~ ., data = swiss, family = gaussian())
  add_m1 <- add1(m1, ~ I(Education^2) + .^2, test = "Chisq")

  m2 <- fglm(Fertility ~ ., data = swiss, family = gaussian())
  add_m2 <- add1(m2, ~ I(Education^2) + .^2, test = "Chisq")

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`F value`, add_m2$`F value`)
  expect_equal(add_m1$`Pr(>F)`, add_m2$`Pr(>F)`)
})

test_that("Add: fglm (gaussian) == glm with LRT test", {
  m1 <- glm(Fertility ~ ., data = swiss, family = gaussian())
  add_m1 <- add1(m1, ~ I(Education^2) + .^2, test = "LRT")

  m2 <- fglm(Fertility ~ ., data = swiss, family = gaussian())
  add_m2 <- add1(m2, ~ I(Education^2) + .^2, test = "LRT")

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`F value`, add_m2$`F value`)
  expect_equal(add_m1$`Pr(>F)`, add_m2$`Pr(>F)`)
})

test_that("Add: fglm (gaussian) == glm with Rao test", {
  m1 <- glm(Fertility ~ ., data = swiss, family = gaussian())
  add_m1 <- add1(m1, ~ I(Education^2) + .^2, test = "Rao")

  m2 <- fglm(Fertility ~ ., data = swiss, family = gaussian())
  add_m2 <- add1(m2, ~ I(Education^2) + .^2, test = "Rao")

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`scaled Rao sc.`, add_m2$`scaled Rao sc.`)
  expect_equal(add_m1$`Pr(>Chi)`, add_m2$`Pr(>Chi)`)
})

# Quasi-Poisson ----

test_that("Add: fglm (quasipoisson) == glm with no test", {
  m1 <- glm(Fertility ~ ., data = swiss, family = quasipoisson())
  add_m1 <- add1(m1, ~ I(Education^2) + .^2)

  m2 <- fglm(Fertility ~ ., data = swiss, family = quasipoisson())
  add_m2 <- add1(m2, ~ I(Education^2) + .^2)

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
})

test_that("Add: fglm (quasipoisson) == glm with F test", {
  m1 <- glm(Fertility ~ ., data = swiss, family = quasipoisson())
  add_m1 <- add1(m1, ~ I(Education^2) + .^2, test = "F")

  m2 <- fglm(Fertility ~ ., data = swiss, family = quasipoisson())
  add_m2 <- add1(m2, ~ I(Education^2) + .^2, test = "F")

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`F value`, add_m2$`F value`)
  expect_equal(add_m1$`Pr(>F)`, add_m2$`Pr(>F)`)
})

test_that("Add: fglm (quasipoisson) == glm with Chisq test", {
  m1 <- glm(Fertility ~ ., data = swiss, family = quasipoisson())
  add_m1 <- add1(m1, ~ I(Education^2) + .^2, test = "Chisq")

  m2 <- fglm(Fertility ~ ., data = swiss, family = quasipoisson())
  add_m2 <- add1(m2, ~ I(Education^2) + .^2, test = "Chisq")

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`F value`, add_m2$`F value`)
  expect_equal(add_m1$`Pr(>F)`, add_m2$`Pr(>F)`)
})

test_that("Add: fglm (quasipoisson) == glm with LRT test", {
  m1 <- glm(Fertility ~ ., data = swiss, family = quasipoisson())
  add_m1 <- add1(m1, ~ I(Education^2) + .^2, test = "LRT")

  m2 <- fglm(Fertility ~ ., data = swiss, family = quasipoisson())
  add_m2 <- add1(m2, ~ I(Education^2) + .^2, test = "LRT")

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`F value`, add_m2$`F value`)
  expect_equal(add_m1$`Pr(>F)`, add_m2$`Pr(>F)`)
})

test_that("Add: fglm (quasipoisson) == glm with Rao test", {
  m1 <- glm(Fertility ~ ., data = swiss, family = quasipoisson())
  add_m1 <- add1(m1, ~ I(Education^2) + .^2, test = "Rao")

  m2 <- fglm(Fertility ~ ., data = swiss, family = quasipoisson())
  add_m2 <- add1(m2, ~ I(Education^2) + .^2, test = "Rao")

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`scaled Rao sc.`, add_m2$`scaled Rao sc.`)
  expect_equal(add_m1$`Pr(>Chi)`, add_m2$`Pr(>Chi)`)
})
