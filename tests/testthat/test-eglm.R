patrick::with_parameters_test_that("eglm fitting is the same as glm:", {
  m1 <- glm(model, family = family, data = mtcars, x = TRUE)
  m2 <- eglm(model, family = family, data = mtcars, x = TRUE, reduce = reduce)
  expect_model_equal(m2, m1)
},
.cases = make_eglm_cases()
)
