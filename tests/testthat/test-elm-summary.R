patrick::with_parameters_test_that("elm summary is the same as lm:", {
  m1 <- summary(lm(model, data = mtcars))
  m2 <- summary(elm(model, data = mtcars, reduce = reduce))
  expect_summary_equal(m2, m1)
},
.cases = elm_cases()
)
