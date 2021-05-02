patrick::with_parameters_test_that("eglm fitting is the same as glm:", {
  m1 <- lm(model, data = mtcars, x = TRUE)
  m2 <- elm(model, data = mtcars, x = TRUE, reduce = reduce)
  expect_model_equal(m2, m1)
},
.cases = elm_cases()
)
