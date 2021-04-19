# Gaussian ----

test_that("Drop: flgm (gaussian) == lm with no test", {
  m1 <- lm(mpg ~ wt + am, data = mtcars)
  drop_m1 <- drop1(m1)

  m2 <- elm(mpg ~ wt + am, data = mtcars, reduce = FALSE)
  drop_m2 <- drop1(m2)

  m3 <- elm(mpg ~ wt + am, data = mtcars, reduce = TRUE)
  drop_m3 <- drop1(m3)

  expect_equal(drop_m2$Df, drop_m1$Df)
  expect_equal(drop_m2$Deviance, drop_m1$Deviance)
  expect_equal(drop_m2$AIC, drop_m1$AIC)

  expect_equal(drop_m2$Df, drop_m3$Df)
  expect_equal(drop_m2$Deviance, drop_m3$Deviance)
  expect_equal(drop_m2$AIC, drop_m3$AIC)
})

test_that("Drop: flgm (gaussian) == lm with F test", {
  m1 <- lm(mpg ~ wt + am, data = mtcars)
  drop_m1 <- drop1(m1, test = "F")

  m2 <- elm(mpg ~ wt + am, data = mtcars, reduce = FALSE)
  drop_m2 <- drop1(m2, test = "F")

  m3 <- elm(mpg ~ wt + am, data = mtcars, reduce = TRUE)
  drop_m3 <- drop1(m3, test = "F")

  expect_equal(drop_m2$Df, drop_m1$Df)
  expect_equal(drop_m2$Deviance, drop_m1$Deviance)
  expect_equal(drop_m2$AIC, drop_m1$AIC)
  expect_equal(drop_m2$`F value`, drop_m1$`F value`)
  expect_equal(drop_m2$`Pr(>F)`, drop_m1$`Pr(>F)`)

  expect_equal(drop_m2$Df, drop_m3$Df)
  expect_equal(drop_m2$Deviance, drop_m3$Deviance)
  expect_equal(drop_m2$AIC, drop_m3$AIC)
  expect_equal(drop_m2$`F value`, drop_m3$`F value`)
  expect_equal(drop_m2$`Pr(>F)`, drop_m3$`Pr(>F)`)
})

test_that("Drop: flgm (gaussian) == lm with Chisq test", {
  m1 <- lm(mpg ~ wt + am, data = mtcars)
  drop_m1 <- drop1(m1, test = "Chisq")

  m2 <- elm(mpg ~ wt + am, data = mtcars, reduce = FALSE)
  drop_m2 <- drop1(m2, test = "Chisq")

  m3 <- elm(mpg ~ wt + am, data = mtcars, reduce = TRUE)
  drop_m3 <- drop1(m3, test = "Chisq")

  expect_equal(drop_m2$Df, drop_m1$Df)
  expect_equal(drop_m2$Deviance, drop_m1$Deviance)
  expect_equal(drop_m2$AIC, drop_m1$AIC)
  expect_equal(drop_m2$`F value`, drop_m1$`F value`)
  expect_equal(drop_m2$`Pr(>F)`, drop_m1$`Pr(>F)`)

  expect_equal(drop_m3$Df, drop_m1$Df)
  expect_equal(drop_m3$Deviance, drop_m1$Deviance)
  expect_equal(drop_m3$AIC, drop_m1$AIC)
  expect_equal(drop_m3$`F value`, drop_m1$`F value`)
  expect_equal(drop_m3$`Pr(>F)`, drop_m1$`Pr(>F)`)
})
