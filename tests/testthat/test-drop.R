# Gaussian ----

test_that("Drop: flgm (gaussian) == glm with no test", {
  m1 <- glm(mpg ~ wt + am, data = mtcars, family = gaussian())
  drop_m1 <- drop1(m1)

  m2 <- eglm(mpg ~ wt + am, data = mtcars, family = gaussian())
  drop_m2 <- drop1(m2)

  expect_equal(drop_m1$Df, drop_m2$Df)
  expect_equal(drop_m1$Deviance, drop_m2$Deviance)
  expect_equal(drop_m1$AIC, drop_m2$AIC)
})

test_that("Drop: flgm (gaussian) == glm with F test", {
  m1 <- glm(mpg ~ wt + am, data = mtcars, family = gaussian())
  drop_m1 <- drop1(m1, test = "F")

  m2 <- eglm(mpg ~ wt + am, data = mtcars, family = gaussian())
  drop_m2 <- drop1(m2, test = "F")

  expect_equal(drop_m1$Df, drop_m2$Df)
  expect_equal(drop_m1$Deviance, drop_m2$Deviance)
  expect_equal(drop_m1$AIC, drop_m2$AIC)
  expect_equal(drop_m1$`F value`, drop_m2$`F value`)
  expect_equal(drop_m1$`Pr(>F)`, drop_m2$`Pr(>F)`)
})

test_that("Drop: flgm (gaussian) == glm with Chisq test", {
  m1 <- glm(mpg ~ wt + am, data = mtcars, family = gaussian())
  drop_m1 <- drop1(m1, test = "Chisq")

  m2 <- eglm(mpg ~ wt + am, data = mtcars, family = gaussian())
  drop_m2 <- drop1(m2, test = "Chisq")

  expect_equal(drop_m1$Df, drop_m2$Df)
  expect_equal(drop_m1$Deviance, drop_m2$Deviance)
  expect_equal(drop_m1$AIC, drop_m2$AIC)
  expect_equal(drop_m1$`F value`, drop_m2$`F value`)
  expect_equal(drop_m1$`Pr(>F)`, drop_m2$`Pr(>F)`)
})

test_that("Drop: flgm (gaussian) == glm with LRT test", {
  m1 <- glm(mpg ~ wt + am, data = mtcars, family = gaussian())
  drop_m1 <- drop1(m1, test = "LRT")

  m2 <- eglm(mpg ~ wt + am, data = mtcars, family = gaussian())
  drop_m2 <- drop1(m2, test = "LRT")

  expect_equal(drop_m1$Df, drop_m2$Df)
  expect_equal(drop_m1$Deviance, drop_m2$Deviance)
  expect_equal(drop_m1$AIC, drop_m2$AIC)
  expect_equal(drop_m1$`scaled dev.`, drop_m2$`scaled dev.`)
  expect_equal(drop_m1$`Pr(>Chi)`, drop_m2$`Pr(>Chi)`)
})

test_that("Drop: flgm (gaussian) == glm with Rao test", {
  m1 <- glm(mpg ~ wt + am, data = mtcars, family = gaussian())
  drop_m1 <- drop1(m1, test = "Rao")

  m2 <- eglm(mpg ~ wt + am, data = mtcars, family = gaussian())
  drop_m2 <- drop1(m2, test = "Rao")

  expect_equal(drop_m1$Df, drop_m2$Df)
  expect_equal(drop_m1$Deviance, drop_m2$Deviance)
  expect_equal(drop_m1$AIC, drop_m2$AIC)
  expect_equal(drop_m1$`scaled Rao sc.`, drop_m2$`scaled Rao sc.`)
  expect_equal(drop_m1$`Pr(>Chi)`, drop_m2$`Pr(>Chi)`)
})

# Inverse-Gaussian ----

test_that("Drop: flgm (inverse.gaussian) == glm with no test", {
  m1 <- glm(mpg ~ wt + am, data = mtcars, family = inverse.gaussian())
  drop_m1 <- drop1(m1)

  m2 <- eglm(mpg ~ wt + am, data = mtcars, family = inverse.gaussian())
  drop_m2 <- drop1(m2)

  expect_equal(drop_m1$Df, drop_m2$Df)
  expect_equal(drop_m1$Deviance, drop_m2$Deviance)
  expect_equal(drop_m1$AIC, drop_m2$AIC)
})

test_that("Drop: flgm (inverse.gaussian) == glm with F test", {
  m1 <- glm(mpg ~ wt + am, data = mtcars, family = inverse.gaussian())
  drop_m1 <- drop1(m1, test = "F")

  m2 <- eglm(mpg ~ wt + am, data = mtcars, family = inverse.gaussian())
  drop_m2 <- drop1(m2, test = "F")

  expect_equal(drop_m1$Df, drop_m2$Df)
  expect_equal(drop_m1$Deviance, drop_m2$Deviance)
  expect_equal(drop_m1$AIC, drop_m2$AIC)
  expect_equal(drop_m1$`F value`, drop_m2$`F value`)
  expect_equal(drop_m1$`Pr(>F)`, drop_m2$`Pr(>F)`)
})

test_that("Drop: flgm (inverse.gaussian) == glm with Chisq test", {
  m1 <- glm(mpg ~ wt + am, data = mtcars, family = inverse.gaussian())
  drop_m1 <- drop1(m1, test = "Chisq")

  m2 <- eglm(mpg ~ wt + am, data = mtcars, family = inverse.gaussian())
  drop_m2 <- drop1(m2, test = "Chisq")

  expect_equal(drop_m1$Df, drop_m2$Df)
  expect_equal(drop_m1$Deviance, drop_m2$Deviance)
  expect_equal(drop_m1$AIC, drop_m2$AIC)
  expect_equal(drop_m1$`F value`, drop_m2$`F value`)
  expect_equal(drop_m1$`Pr(>F)`, drop_m2$`Pr(>F)`)
})

test_that("Drop: flgm (inverse.gaussian) == glm with LRT test", {
  m1 <- glm(mpg ~ wt + am, data = mtcars, family = inverse.gaussian())
  drop_m1 <- drop1(m1, test = "LRT")

  m2 <- eglm(mpg ~ wt + am, data = mtcars, family = inverse.gaussian())
  drop_m2 <- drop1(m2, test = "LRT")

  expect_equal(drop_m1$Df, drop_m2$Df)
  expect_equal(drop_m1$Deviance, drop_m2$Deviance)
  expect_equal(drop_m1$AIC, drop_m2$AIC)
  expect_equal(drop_m1$`scaled dev.`, drop_m2$`scaled dev.`)
  expect_equal(drop_m1$`Pr(>Chi)`, drop_m2$`Pr(>Chi)`)
})

test_that("Drop: flgm (inverse.gaussian) == glm with Rao test", {
  m1 <- glm(mpg ~ wt + am, data = mtcars, family = inverse.gaussian())
  drop_m1 <- drop1(m1, test = "Rao")

  m2 <- eglm(mpg ~ wt + am, data = mtcars, family = inverse.gaussian())
  drop_m2 <- drop1(m2, test = "Rao")

  expect_equal(drop_m1$Df, drop_m2$Df)
  expect_equal(drop_m1$Deviance, drop_m2$Deviance)
  expect_equal(drop_m1$AIC, drop_m2$AIC)
  expect_equal(drop_m1$`scaled Rao sc.`, drop_m2$`scaled Rao sc.`)
  expect_equal(drop_m1$`Pr(>Chi)`, drop_m2$`Pr(>Chi)`)
})

# Binomial ----

test_that("Drop: flgm (binomial) == glm with no test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = binomial())
  drop_m1 <- drop1(m1)

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = binomial())
  drop_m2 <- drop1(m2)

  expect_equal(drop_m1$Df, drop_m2$Df)
  expect_equal(drop_m1$Deviance, drop_m2$Deviance)
  expect_equal(drop_m1$AIC, drop_m2$AIC)
})

test_that("Drop: flgm (binomial) == glm with F test", {
  # F test assumes 'quasibinomial' family
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = binomial())
  drop_m1 <- expect_warning(drop1(m1, test = "F"))

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = binomial())
  drop_m2 <- expect_warning(drop1(m2, test = "F"))

  expect_equal(drop_m1$Df, drop_m2$Df)
  expect_equal(drop_m1$Deviance, drop_m2$Deviance)
  expect_equal(drop_m1$AIC, drop_m2$AIC)
  expect_equal(drop_m1$`F value`, drop_m2$`F value`)
  expect_equal(drop_m1$`Pr(>F)`, drop_m2$`Pr(>F)`)
})

test_that("Drop: flgm (binomial) == glm with Chisq test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = binomial())
  drop_m1 <- drop1(m1, test = "Chisq")

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = binomial())
  drop_m2 <- drop1(m2, test = "Chisq")

  expect_equal(drop_m1$Df, drop_m2$Df)
  expect_equal(drop_m1$Deviance, drop_m2$Deviance)
  expect_equal(drop_m1$AIC, drop_m2$AIC)
  expect_equal(drop_m1$`F value`, drop_m2$`F value`)
  expect_equal(drop_m1$`Pr(>F)`, drop_m2$`Pr(>F)`)
})

test_that("Drop: flgm (binomial) == glm with LRT test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = binomial())
  drop_m1 <- drop1(m1, test = "LRT")

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = binomial())
  drop_m2 <- drop1(m2, test = "LRT")

  expect_equal(drop_m1$Df, drop_m2$Df)
  expect_equal(drop_m1$Deviance, drop_m2$Deviance)
  expect_equal(drop_m1$AIC, drop_m2$AIC)
  expect_equal(drop_m1$`scaled dev.`, drop_m2$`scaled dev.`)
  expect_equal(drop_m1$`Pr(>Chi)`, drop_m2$`Pr(>Chi)`)
})

test_that("Drop: flgm (binomial) == glm with Rao test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = binomial())
  drop_m1 <- drop1(m1, test = "Rao")

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = binomial())
  drop_m2 <- drop1(m2, test = "Rao")

  expect_equal(drop_m1$Df, drop_m2$Df)
  expect_equal(drop_m1$Deviance, drop_m2$Deviance)
  expect_equal(drop_m1$AIC, drop_m2$AIC)
  expect_equal(drop_m1$`scaled Rao sc.`, drop_m2$`scaled Rao sc.`)
  expect_equal(drop_m1$`Pr(>Chi)`, drop_m2$`Pr(>Chi)`)
})

# Quasi-Binomial ----

test_that("Drop: flgm (quasibinomial) == glm with no test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = quasibinomial())
  drop_m1 <- drop1(m1)

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = quasibinomial())
  drop_m2 <- drop1(m2)

  expect_equal(drop_m1$Df, drop_m2$Df)
  expect_equal(drop_m1$Deviance, drop_m2$Deviance)
  expect_equal(drop_m1$AIC, drop_m2$AIC)
})

test_that("Drop: flgm (quasibinomial) == glm with F test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = quasibinomial())
  drop_m1 <- drop1(m1, test = "F")

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = quasibinomial())
  drop_m2 <- drop1(m2, test = "F")

  expect_equal(drop_m1$Df, drop_m2$Df)
  expect_equal(drop_m1$Deviance, drop_m2$Deviance)
  expect_equal(drop_m1$AIC, drop_m2$AIC)
  expect_equal(drop_m1$`F value`, drop_m2$`F value`)
  expect_equal(drop_m1$`Pr(>F)`, drop_m2$`Pr(>F)`)
})

test_that("Drop: flgm (quasibinomial) == glm with Chisq test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = quasibinomial())
  drop_m1 <- drop1(m1, test = "Chisq")

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = quasibinomial())
  drop_m2 <- drop1(m2, test = "Chisq")

  expect_equal(drop_m1$Df, drop_m2$Df)
  expect_equal(drop_m1$Deviance, drop_m2$Deviance)
  expect_equal(drop_m1$AIC, drop_m2$AIC)
  expect_equal(drop_m1$`F value`, drop_m2$`F value`)
  expect_equal(drop_m1$`Pr(>F)`, drop_m2$`Pr(>F)`)
})

test_that("Drop: flgm (quasibinomial) == glm with LRT test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = quasibinomial())
  drop_m1 <- drop1(m1, test = "LRT")

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = quasibinomial())
  drop_m2 <- drop1(m2, test = "LRT")

  expect_equal(drop_m1$Df, drop_m2$Df)
  expect_equal(drop_m1$Deviance, drop_m2$Deviance)
  expect_equal(drop_m1$AIC, drop_m2$AIC)
  expect_equal(drop_m1$`scaled dev.`, drop_m2$`scaled dev.`)
  expect_equal(drop_m1$`Pr(>Chi)`, drop_m2$`Pr(>Chi)`)
})

test_that("Drop: flgm (quasibinomial) == glm with Rao test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = quasibinomial())
  drop_m1 <- drop1(m1, test = "Rao")

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = quasibinomial())
  drop_m2 <- drop1(m2, test = "Rao")

  expect_equal(drop_m1$Df, drop_m2$Df)
  expect_equal(drop_m1$Deviance, drop_m2$Deviance)
  expect_equal(drop_m1$AIC, drop_m2$AIC)
  expect_equal(drop_m1$`scaled Rao sc.`, drop_m2$`scaled Rao sc.`)
  expect_equal(drop_m1$`Pr(>Chi)`, drop_m2$`Pr(>Chi)`)
})

# Poisson ----

test_that("Drop: flgm (poisson) == glm with no test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = poisson())
  drop_m1 <- drop1(m1)

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = poisson())
  drop_m2 <- drop1(m2)

  expect_equal(drop_m1$Df, drop_m2$Df)
  expect_equal(drop_m1$Deviance, drop_m2$Deviance)
  expect_equal(drop_m1$AIC, drop_m2$AIC)
})

test_that("Drop: flgm (poisson) == glm with F test", {
  # F test assumes 'quasipoisson' family
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = poisson())
  drop_m1 <- expect_warning(drop1(m1, test = "F"))

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = poisson())
  drop_m2 <- expect_warning(drop1(m2, test = "F"))

  expect_equal(drop_m1$Df, drop_m2$Df)
  expect_equal(drop_m1$Deviance, drop_m2$Deviance)
  expect_equal(drop_m1$AIC, drop_m2$AIC)
  expect_equal(drop_m1$`F value`, drop_m2$`F value`)
  expect_equal(drop_m1$`Pr(>F)`, drop_m2$`Pr(>F)`)
})

test_that("Drop: flgm (poisson) == glm with Chisq test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = poisson())
  drop_m1 <- drop1(m1, test = "Chisq")

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = poisson())
  drop_m2 <- drop1(m2, test = "Chisq")

  expect_equal(drop_m1$Df, drop_m2$Df)
  expect_equal(drop_m1$Deviance, drop_m2$Deviance)
  expect_equal(drop_m1$AIC, drop_m2$AIC)
  expect_equal(drop_m1$`F value`, drop_m2$`F value`)
  expect_equal(drop_m1$`Pr(>F)`, drop_m2$`Pr(>F)`)
})

test_that("Drop: flgm (poisson) == glm with LRT test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = poisson())
  drop_m1 <- drop1(m1, test = "LRT")

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = poisson())
  drop_m2 <- drop1(m2, test = "LRT")

  expect_equal(drop_m1$Df, drop_m2$Df)
  expect_equal(drop_m1$Deviance, drop_m2$Deviance)
  expect_equal(drop_m1$AIC, drop_m2$AIC)
  expect_equal(drop_m1$`scaled dev.`, drop_m2$`scaled dev.`)
  expect_equal(drop_m1$`Pr(>Chi)`, drop_m2$`Pr(>Chi)`)
})

test_that("Drop: flgm (poisson) == glm with Rao test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = poisson())
  drop_m1 <- drop1(m1, test = "Rao")

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = poisson())
  drop_m2 <- drop1(m2, test = "Rao")

  expect_equal(drop_m1$Df, drop_m2$Df)
  expect_equal(drop_m1$Deviance, drop_m2$Deviance)
  expect_equal(drop_m1$AIC, drop_m2$AIC)
  expect_equal(drop_m1$`scaled Rao sc.`, drop_m2$`scaled Rao sc.`)
  expect_equal(drop_m1$`Pr(>Chi)`, drop_m2$`Pr(>Chi)`)
})

# Quasi-Poisson ----

test_that("Drop: flgm (poisson) == glm with no test", {
  m1 <- glm(mpg ~ wt + am, data = mtcars, family = quasipoisson())
  drop_m1 <- drop1(m1)

  m2 <- eglm(mpg ~ wt + am, data = mtcars, family = quasipoisson())
  drop_m2 <- drop1(m2)

  expect_equal(drop_m1$Df, drop_m2$Df)
  expect_equal(drop_m1$Deviance, drop_m2$Deviance)
  expect_equal(drop_m1$AIC, drop_m2$AIC)
})

test_that("Drop: flgm (poisson) == glm with F test", {
  m1 <- glm(mpg ~ wt + am, data = mtcars, family = quasipoisson())
  drop_m1 <- drop1(m1, test = "F")

  m2 <- eglm(mpg ~ wt + am, data = mtcars, family = quasipoisson())
  drop_m2 <- drop1(m2, test = "F")

  expect_equal(drop_m1$Df, drop_m2$Df)
  expect_equal(drop_m1$Deviance, drop_m2$Deviance)
  expect_equal(drop_m1$AIC, drop_m2$AIC)
  expect_equal(drop_m1$`F value`, drop_m2$`F value`)
  expect_equal(drop_m1$`Pr(>F)`, drop_m2$`Pr(>F)`)
})

test_that("Drop: flgm (poisson) == glm with Chisq test", {
  m1 <- glm(mpg ~ wt + am, data = mtcars, family = quasipoisson())
  drop_m1 <- drop1(m1, test = "Chisq")

  m2 <- eglm(mpg ~ wt + am, data = mtcars, family = quasipoisson())
  drop_m2 <- drop1(m2, test = "Chisq")

  expect_equal(drop_m1$Df, drop_m2$Df)
  expect_equal(drop_m1$Deviance, drop_m2$Deviance)
  expect_equal(drop_m1$AIC, drop_m2$AIC)
  expect_equal(drop_m1$`F value`, drop_m2$`F value`)
  expect_equal(drop_m1$`Pr(>F)`, drop_m2$`Pr(>F)`)
})

test_that("Drop: flgm (poisson) == glm with LRT test", {
  m1 <- glm(mpg ~ wt + am, data = mtcars, family = quasipoisson())
  drop_m1 <- drop1(m1, test = "LRT")

  m2 <- eglm(mpg ~ wt + am, data = mtcars, family = quasipoisson())
  drop_m2 <- drop1(m2, test = "LRT")

  expect_equal(drop_m1$Df, drop_m2$Df)
  expect_equal(drop_m1$Deviance, drop_m2$Deviance)
  expect_equal(drop_m1$AIC, drop_m2$AIC)
  expect_equal(drop_m1$`scaled dev.`, drop_m2$`scaled dev.`)
  expect_equal(drop_m1$`Pr(>Chi)`, drop_m2$`Pr(>Chi)`)
})

test_that("Drop: flgm (poisson) == glm with Rao test", {
  m1 <- glm(mpg ~ wt + am, data = mtcars, family = quasipoisson())
  drop_m1 <- drop1(m1, test = "Rao")

  m2 <- eglm(mpg ~ wt + am, data = mtcars, family = quasipoisson())
  drop_m2 <- drop1(m2, test = "Rao")

  expect_equal(drop_m1$Df, drop_m2$Df)
  expect_equal(drop_m1$Deviance, drop_m2$Deviance)
  expect_equal(drop_m1$AIC, drop_m2$AIC)
  expect_equal(drop_m1$`scaled Rao sc.`, drop_m2$`scaled Rao sc.`)
  expect_equal(drop_m1$`Pr(>Chi)`, drop_m2$`Pr(>Chi)`)
})

# Quasi ----

test_that("Drop: flgm (quasi) == glm with no test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = quasi())
  drop_m1 <- drop1(m1)

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = quasi())
  drop_m2 <- drop1(m2)

  expect_equal(drop_m1$Df, drop_m2$Df)
  expect_equal(drop_m1$Deviance, drop_m2$Deviance)
  expect_equal(drop_m1$AIC, drop_m2$AIC)
})

test_that("Drop: flgm (quasi) == glm with F test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = quasi())
  drop_m1 <- drop1(m1, test = "F")

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = quasi())
  drop_m2 <- drop1(m2, test = "F")

  expect_equal(drop_m1$Df, drop_m2$Df)
  expect_equal(drop_m1$Deviance, drop_m2$Deviance)
  expect_equal(drop_m1$AIC, drop_m2$AIC)
  expect_equal(drop_m1$`F value`, drop_m2$`F value`)
  expect_equal(drop_m1$`Pr(>F)`, drop_m2$`Pr(>F)`)
})

test_that("Drop: flgm (quasi) == glm with Chisq test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = quasi())
  drop_m1 <- drop1(m1, test = "Chisq")

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = quasi())
  drop_m2 <- drop1(m2, test = "Chisq")

  expect_equal(drop_m1$Df, drop_m2$Df)
  expect_equal(drop_m1$Deviance, drop_m2$Deviance)
  expect_equal(drop_m1$AIC, drop_m2$AIC)
  expect_equal(drop_m1$`F value`, drop_m2$`F value`)
  expect_equal(drop_m1$`Pr(>F)`, drop_m2$`Pr(>F)`)
})

test_that("Drop: flgm (quasi) == glm with LRT test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = quasi())
  drop_m1 <- drop1(m1, test = "LRT")

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = quasi())
  drop_m2 <- drop1(m2, test = "LRT")

  expect_equal(drop_m1$Df, drop_m2$Df)
  expect_equal(drop_m1$Deviance, drop_m2$Deviance)
  expect_equal(drop_m1$AIC, drop_m2$AIC)
  expect_equal(drop_m1$`scaled dev.`, drop_m2$`scaled dev.`)
  expect_equal(drop_m1$`Pr(>Chi)`, drop_m2$`Pr(>Chi)`)
})

test_that("Drop: flgm (quasi) == glm with Rao test", {
  m1 <- glm(am ~ wt + mpg, data = mtcars, family = quasi())
  drop_m1 <- drop1(m1, test = "Rao")

  m2 <- eglm(am ~ wt + mpg, data = mtcars, family = quasi())
  drop_m2 <- drop1(m2, test = "Rao")

  expect_equal(drop_m1$Df, drop_m2$Df)
  expect_equal(drop_m1$Deviance, drop_m2$Deviance)
  expect_equal(drop_m1$AIC, drop_m2$AIC)
  expect_equal(drop_m1$`scaled Rao sc.`, drop_m2$`scaled Rao sc.`)
  expect_equal(drop_m1$`Pr(>Chi)`, drop_m2$`Pr(>Chi)`)
})

# Sanity check ----

test_that("Drop: fails without model matrix", {
  m1 <- eglm(mpg ~ wt + am, data = mtcars, family = gaussian(), model = FALSE)
  expect_error(drop1(m1))
})
