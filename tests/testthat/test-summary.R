# LM ----

patrick::with_parameters_test_that("elm summary is the same as lm:", {
  m1 <- summary(lm(model, data = mtcars))
  m2 <- summary(elm(model, data = mtcars, reduce = reduce))
  expect_summary_equal(m2, m1)
},
.cases = elm_cases()
)

# GLM ----

patrick::with_parameters_test_that("eglm fitting is the same as glm:", {
  m1 <- summary(glm(model, family = family, data = mtcars))
  m2 <- summary(eglm(model, family = family, data = mtcars, reduce = reduce))
  expect_summary_equal(m2, m1)
},
.cases = eglm_cases()
)
