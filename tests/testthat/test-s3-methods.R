test_that("summary.fglm fails with non-fglm objects", {
  expect_error(hermes:::summary.fglm(list()))
})
