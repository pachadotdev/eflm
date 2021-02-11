# Gaussian ----

test_that("Add: eglm (gaussian) == glm with no test", {
  m1 <- glm(mpg ~ wt, data = mtcars, family = gaussian())
  add_m1 <- add1(m1, ~ . + am)

  m2 <- eglm(mpg ~ wt, data = mtcars, family = gaussian())
  add_m2 <- add1(m2, ~ . +  am)

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
})

test_that("Add: eglm (gaussian) == glm with F test", {
  m1 <- glm(mpg ~ wt, data = mtcars, family = gaussian())
  add_m1 <- add1(m1, ~ . +  am, test = "F")

  m2 <- eglm(mpg ~ wt, data = mtcars, family = gaussian())
  add_m2 <- add1(m2, ~ . +  am, test = "F")

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`F value`, add_m2$`F value`)
  expect_equal(add_m1$`Pr(>F)`, add_m2$`Pr(>F)`)
})

test_that("Add: eglm (gaussian) == glm with Chisq test", {
  m1 <- glm(mpg ~ wt, data = mtcars, family = gaussian())
  add_m1 <- add1(m1, ~ . +  am, test = "Chisq")

  m2 <- eglm(mpg ~ wt, data = mtcars, family = gaussian())
  add_m2 <- add1(m2, ~ . +  am, test = "Chisq")

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`F value`, add_m2$`F value`)
  expect_equal(add_m1$`Pr(>F)`, add_m2$`Pr(>F)`)
})

test_that("Add: eglm (gaussian) == glm with LRT test", {
  m1 <- glm(mpg ~ wt, data = mtcars, family = gaussian())
  add_m1 <- add1(m1, ~ . +  am, test = "LRT")

  m2 <- eglm(mpg ~ wt, data = mtcars, family = gaussian())
  add_m2 <- add1(m2, ~ . +  am, test = "LRT")

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`F value`, add_m2$`F value`)
  expect_equal(add_m1$`Pr(>F)`, add_m2$`Pr(>F)`)
})

test_that("Add: eglm (gaussian) == glm with Rao test", {
  m1 <- glm(mpg ~ wt, data = mtcars, family = gaussian())
  add_m1 <- add1(m1, ~ . +  am, test = "Rao")

  m2 <- eglm(mpg ~ wt, data = mtcars, family = gaussian())
  add_m2 <- add1(m2, ~ . +  am, test = "Rao")

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`scaled Rao sc.`, add_m2$`scaled Rao sc.`)
  expect_equal(add_m1$`Pr(>Chi)`, add_m2$`Pr(>Chi)`)
})

# Inverse-Gaussian ----

test_that("Add: eglm (inverse.gaussian) == glm with no test", {
  m1 <- glm(mpg ~ wt, data = mtcars, family = inverse.gaussian())
  add_m1 <- add1(m1, ~ . +  am)

  m2 <- eglm(mpg ~ wt, data = mtcars, family = inverse.gaussian())
  add_m2 <- add1(m2, ~ . +  am)

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
})

test_that("Add: eglm (inverse.gaussian) == glm with F test", {
  m1 <- glm(mpg ~ wt, data = mtcars, family = inverse.gaussian())
  add_m1 <- add1(m1, ~ . +  am, test = "F")

  m2 <- eglm(mpg ~ wt, data = mtcars, family = inverse.gaussian())
  add_m2 <- add1(m2, ~ . +  am, test = "F")

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`F value`, add_m2$`F value`)
  expect_equal(add_m1$`Pr(>F)`, add_m2$`Pr(>F)`)
})

test_that("Add: eglm (inverse.gaussian) == glm with Chisq test", {
  m1 <- glm(mpg ~ wt, data = mtcars, family = inverse.gaussian())
  add_m1 <- add1(m1, ~ . +  am, test = "Chisq")

  m2 <- eglm(mpg ~ wt, data = mtcars, family = inverse.gaussian())
  add_m2 <- add1(m2, ~ . +  am, test = "Chisq")

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`F value`, add_m2$`F value`)
  expect_equal(add_m1$`Pr(>F)`, add_m2$`Pr(>F)`)
})

test_that("Add: eglm (inverse.gaussian) == glm with LRT test", {
  m1 <- glm(mpg ~ wt, data = mtcars, family = inverse.gaussian())
  add_m1 <- add1(m1, ~ . +  am, test = "LRT")

  m2 <- eglm(mpg ~ wt, data = mtcars, family = inverse.gaussian())
  add_m2 <- add1(m2, ~ . +  am, test = "LRT")

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`F value`, add_m2$`F value`)
  expect_equal(add_m1$`Pr(>F)`, add_m2$`Pr(>F)`)
})

test_that("Add: eglm (inverse.gaussian) == glm with Rao test", {
  m1 <- glm(mpg ~ wt, data = mtcars, family = inverse.gaussian())
  add_m1 <- add1(m1, ~ . +  am, test = "Rao")

  m2 <- eglm(mpg ~ wt, data = mtcars, family = inverse.gaussian())
  add_m2 <- add1(m2, ~ . +  am, test = "Rao")

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`scaled Rao sc.`, add_m2$`scaled Rao sc.`)
  expect_equal(add_m1$`Pr(>Chi)`, add_m2$`Pr(>Chi)`)
})

# Binomial ----

test_that("Add: eglm (binomial) == glm with no test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = binomial())
  add_m1 <- add1(m1, ~ I(mpg^2) + .^2)

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = binomial())
  add_m2 <- add1(m2, ~ I(mpg^2) + .^2)

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
})

test_that("Add: eglm (binomial) == glm with F test", {
  # F test assumes 'quasibinomial' family
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = binomial())
  add_m1 <- expect_warning(add1(m1, ~ I(mpg^2) + .^2, test = "F"))

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = binomial())
  add_m2 <- expect_warning(add1(m2, ~ I(mpg^2) + .^2, test = "F"))

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`F value`, add_m2$`F value`)
  expect_equal(add_m1$`Pr(>F)`, add_m2$`Pr(>F)`)
})

test_that("Add: eglm (binomial) == glm with Chisq test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = binomial())
  add_m1 <- add1(m1, ~ I(mpg^2) + .^2, test = "Chisq")

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = binomial())
  add_m2 <- add1(m2, ~ I(mpg^2) + .^2, test = "Chisq")

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`F value`, add_m2$`F value`)
  expect_equal(add_m1$`Pr(>F)`, add_m2$`Pr(>F)`)
})

test_that("Add: eglm (binomial) == glm with LRT test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = binomial())
  add_m1 <- add1(m1, ~ I(mpg^2) + .^2, test = "LRT")

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = binomial())
  add_m2 <- add1(m2, ~ I(mpg^2) + .^2, test = "LRT")

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`F value`, add_m2$`F value`)
  expect_equal(add_m1$`Pr(>F)`, add_m2$`Pr(>F)`)
})

test_that("Add: eglm (binomial) == glm with Rao test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = binomial())
  add_m1 <- add1(m1, ~ I(mpg^2) + .^2, test = "Rao")

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = binomial())
  add_m2 <- add1(m2, ~ I(mpg^2) + .^2, test = "Rao")

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`scaled Rao sc.`, add_m2$`scaled Rao sc.`)
  expect_equal(add_m1$`Pr(>Chi)`, add_m2$`Pr(>Chi)`)
})

# Quasi-Binomial ----

test_that("Add: eglm (quasibinomial) == glm with no test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = quasibinomial())
  add_m1 <- add1(m1, ~ I(mpg^2) + .^2)

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = quasibinomial())
  add_m2 <- add1(m2, ~ I(mpg^2) + .^2)

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
})

test_that("Add: eglm (quasibinomial) == glm with F test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = quasibinomial())
  add_m1 <- add1(m1, ~ I(mpg^2) + .^2, test = "F")

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = quasibinomial())
  add_m2 <- add1(m2, ~ I(mpg^2) + .^2, test = "F")

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`F value`, add_m2$`F value`)
  expect_equal(add_m1$`Pr(>F)`, add_m2$`Pr(>F)`)
})

test_that("Add: eglm (quasibinomial) == glm with Chisq test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = quasibinomial())
  add_m1 <- add1(m1, ~ I(mpg^2) + .^2, test = "Chisq")

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = quasibinomial())
  add_m2 <- add1(m2, ~ I(mpg^2) + .^2, test = "Chisq")

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`F value`, add_m2$`F value`)
  expect_equal(add_m1$`Pr(>F)`, add_m2$`Pr(>F)`)
})

test_that("Add: eglm (quasibinomial) == glm with LRT test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = quasibinomial())
  add_m1 <- add1(m1, ~ I(mpg^2) + .^2, test = "LRT")

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = quasibinomial())
  add_m2 <- add1(m2, ~ I(mpg^2) + .^2, test = "LRT")

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`F value`, add_m2$`F value`)
  expect_equal(add_m1$`Pr(>F)`, add_m2$`Pr(>F)`)
})

test_that("Add: eglm (quasibinomial) == glm with Rao test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = quasibinomial())
  add_m1 <- add1(m1, ~ I(mpg^2) + .^2, test = "Rao")

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = quasibinomial())
  add_m2 <- add1(m2, ~ I(mpg^2) + .^2, test = "Rao")

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`scaled Rao sc.`, add_m2$`scaled Rao sc.`)
  expect_equal(add_m1$`Pr(>Chi)`, add_m2$`Pr(>Chi)`)
})

# Poisson ----

test_that("Add: eglm (poisson) == glm with no test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = poisson())
  add_m1 <- add1(m1, ~ I(mpg^2) + .^2)

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = poisson())
  add_m2 <- add1(m2, ~ I(mpg^2) + .^2)

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
})

test_that("Add: eglm (poisson) == glm with F test", {
  # F test assumes 'quasipoisson' family
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = poisson())
  add_m1 <- expect_warning(add1(m1, ~ I(mpg^2) + .^2, test = "F"))

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = poisson())
  add_m2 <- expect_warning(add1(m2, ~ I(mpg^2) + .^2, test = "F"))

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`F value`, add_m2$`F value`)
  expect_equal(add_m1$`Pr(>F)`, add_m2$`Pr(>F)`)
})

test_that("Add: eglm (poisson) == glm with Chisq test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = poisson())
  add_m1 <- add1(m1, ~ I(mpg^2) + .^2, test = "Chisq")

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = poisson())
  add_m2 <- add1(m2, ~ I(mpg^2) + .^2, test = "Chisq")

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`F value`, add_m2$`F value`)
  expect_equal(add_m1$`Pr(>F)`, add_m2$`Pr(>F)`)
})

test_that("Add: eglm (poisson) == glm with LRT test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = poisson())
  add_m1 <- add1(m1, ~ I(mpg^2) + .^2, test = "LRT")

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = poisson())
  add_m2 <- add1(m2, ~ I(mpg^2) + .^2, test = "LRT")

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`F value`, add_m2$`F value`)
  expect_equal(add_m1$`Pr(>F)`, add_m2$`Pr(>F)`)
})

test_that("Add: eglm (poisson) == glm with Rao test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = poisson())
  add_m1 <- add1(m1, ~ I(mpg^2) + .^2, test = "Rao")

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = poisson())
  add_m2 <- add1(m2, ~ I(mpg^2) + .^2, test = "Rao")

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`scaled Rao sc.`, add_m2$`scaled Rao sc.`)
  expect_equal(add_m1$`Pr(>Chi)`, add_m2$`Pr(>Chi)`)
})

# Quasi-Poisson ----

test_that("Add: eglm (quasipoisson) == glm with no test", {
  m1 <- glm(mpg ~ wt, data = mtcars, family = quasipoisson())
  add_m1 <- add1(m1, ~ . +  am)

  m2 <- eglm(mpg ~ wt, data = mtcars, family = quasipoisson())
  add_m2 <- add1(m2, ~ . +  am)

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
})

test_that("Add: eglm (quasipoisson) == glm with F test", {
  m1 <- glm(mpg ~ wt, data = mtcars, family = quasipoisson())
  add_m1 <- add1(m1, ~ . +  am, test = "F")

  m2 <- eglm(mpg ~ wt, data = mtcars, family = quasipoisson())
  add_m2 <- add1(m2, ~ . +  am, test = "F")

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`F value`, add_m2$`F value`)
  expect_equal(add_m1$`Pr(>F)`, add_m2$`Pr(>F)`)
})

test_that("Add: eglm (quasipoisson) == glm with Chisq test", {
  m1 <- glm(mpg ~ wt, data = mtcars, family = quasipoisson())
  add_m1 <- add1(m1, ~ . +  am, test = "Chisq")

  m2 <- eglm(mpg ~ wt, data = mtcars, family = quasipoisson())
  add_m2 <- add1(m2, ~ . +  am, test = "Chisq")

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`F value`, add_m2$`F value`)
  expect_equal(add_m1$`Pr(>F)`, add_m2$`Pr(>F)`)
})

test_that("Add: eglm (quasipoisson) == glm with LRT test", {
  m1 <- glm(mpg ~ wt, data = mtcars, family = quasipoisson())
  add_m1 <- add1(m1, ~ . +  am, test = "LRT")

  m2 <- eglm(mpg ~ wt, data = mtcars, family = quasipoisson())
  add_m2 <- add1(m2, ~ . +  am, test = "LRT")

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`F value`, add_m2$`F value`)
  expect_equal(add_m1$`Pr(>F)`, add_m2$`Pr(>F)`)
})

test_that("Add: eglm (quasipoisson) == glm with Rao test", {
  m1 <- glm(mpg ~ wt, data = mtcars, family = quasipoisson())
  add_m1 <- add1(m1, ~ . +  am, test = "Rao")

  m2 <- eglm(mpg ~ wt, data = mtcars, family = quasipoisson())
  add_m2 <- add1(m2, ~ . +  am, test = "Rao")

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`scaled Rao sc.`, add_m2$`scaled Rao sc.`)
  expect_equal(add_m1$`Pr(>Chi)`, add_m2$`Pr(>Chi)`)
})

# Quasi ----

test_that("Add: eglm (quasi) == glm with no test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = quasi())
  add_m1 <- add1(m1, ~ I(mpg^2) + .^2)

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = quasi())
  add_m2 <- add1(m2, ~ I(mpg^2) + .^2)

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
})

test_that("Add: eglm (quasi) == glm with F test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = quasi())
  add_m1 <- add1(m1, ~ I(mpg^2) + .^2, test = "F")

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = quasi())
  add_m2 <- add1(m2, ~ I(mpg^2) + .^2, test = "F")

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`F value`, add_m2$`F value`)
  expect_equal(add_m1$`Pr(>F)`, add_m2$`Pr(>F)`)
})

test_that("Add: eglm (quasi) == glm with Chisq test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = quasi())
  add_m1 <- add1(m1, ~ I(mpg^2) + .^2, test = "Chisq")

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = quasi())
  add_m2 <- add1(m2, ~ I(mpg^2) + .^2, test = "Chisq")

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`F value`, add_m2$`F value`)
  expect_equal(add_m1$`Pr(>F)`, add_m2$`Pr(>F)`)
})

test_that("Add: eglm (quasi) == glm with LRT test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = quasi())
  add_m1 <- add1(m1, ~ I(mpg^2) + .^2, test = "LRT")

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = quasi())
  add_m2 <- add1(m2, ~ I(mpg^2) + .^2, test = "LRT")

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`F value`, add_m2$`F value`)
  expect_equal(add_m1$`Pr(>F)`, add_m2$`Pr(>F)`)
})

test_that("Add: eglm (quasi) == glm with Rao test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = quasi())
  add_m1 <- add1(m1, ~ I(mpg^2) + .^2, test = "Rao")

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = quasi())
  add_m2 <- add1(m2, ~ I(mpg^2) + .^2, test = "Rao")

  expect_equal(add_m1$Df, add_m2$Df)
  expect_equal(add_m1$Deviance, add_m2$Deviance)
  expect_equal(add_m1$AIC, add_m2$AIC)
  expect_equal(add_m1$`scaled Rao sc.`, add_m2$`scaled Rao sc.`)
  expect_equal(add_m1$`Pr(>Chi)`, add_m2$`Pr(>Chi)`)
})

# Sanity checks ----

test_that("Add: eglm fails without model matrix", {
  m1 <- eglm(mpg ~ wt, data = mtcars, family = gaussian(), model = FALSE)
  expect_error(add1(m1, ~ . + am))
})
