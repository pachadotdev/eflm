test_that("eglm + vcovCL return the same as glm + vcovCL", {
  m1 <- glm(mpg ~ wt, family = gaussian(), data = mtcars)
  m2 <- eglm(mpg ~ wt, family = gaussian(), data = mtcars)

  vcov_m1 <- sandwich::vcovCL(m1, cluster = NULL)
  vcov_m2 <- sandwich::vcovCL(m2, cluster = NULL)

  expect_equal(vcov_m1, vcov_m2)

  vcov_m1 <- sandwich::vcovCL(m1, cluster = eval(m1$call$data)[,"cyl"])
  vcov_m2 <- sandwich::vcovCL(m2, cluster = eval(m1$call$data)[,"cyl"])

  expect_equal(vcov_m1, vcov_m2)
})
