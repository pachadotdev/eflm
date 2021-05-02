# LM ----

patrick::with_parameters_test_that("elm + add is the same as lm:", {
  m1 <- lm(model, data = mtcars)
  m2 <- elm(model, data = mtcars, reduce = reduce)

  am1 <- add1(m1, ~ . + cyl, test = test)
  am2 <- add1(m2, ~ . + cyl, test = test)

  dm1 <- drop1(m1, test = test)
  dm2 <- drop1(m2, test = test)

  expect_add1_drop1_equal(am2, am1)
  expect_add1_drop1_equal(dm2, dm1)
},
.cases = elm_add1_cases()
)

# GLM ----

patrick::with_parameters_test_that("eglm + add is the same as glm:", {
  m1 <- glm(model, data = mtcars)
  m2 <- eglm(model, data = mtcars, reduce = reduce)

  am1 <- add1(m1, ~ . + cyl, test = test)
  am2 <- add1(m2, ~ . + cyl, test = test)

  dm1 <- drop1(m1, test = test)
  dm2 <- drop1(m2, test = test)

  expect_add1_drop1_equal(am2, am1)
  expect_add1_drop1_equal(dm2, dm1)
},
.cases = elm_add1_cases()
)
