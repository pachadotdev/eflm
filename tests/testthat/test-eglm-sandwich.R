# test_that("eglm + vcovCL return the same as glm + vcovCL", {
#   m1 <- glm(mpg ~ wt, family = gaussian, data = mtcars)
#   m2 <- eglm(mpg ~ wt, family = gaussian, data = mtcars, reduce = F)
#
#   vcov_m1 <- sandwich::vcovCL(m1, cluster = NULL)
#   vcov_m2 <- sandwich::vcovCL(m2, cluster = NULL)
#
#   expect_equal(vcov_m2, vcov_m1)
#
#   vcov_m1 <- sandwich::vcovCL(m1, cluster = eval(m1$call$data)[, "cyl"])
#   vcov_m2 <- sandwich::vcovCL(m2, cluster = eval(m2$call$data)[, "cyl"])
#
#   expect_equal(vcov_m2, vcov_m1)
# })
#
# test_that("eglm + vcovBS return the same as glm + vcovBS", {
#   # vcovBS uses bootstrap so I need a seed to compare two results!
#   with_seed <- function(seed, code) {
#     code <- substitute(code)
#     set.seed(seed)
#     eval.parent(code)
#   }
#
#   m1 <- glm(mpg ~ wt, family = gaussian, data = mtcars)
#   m2 <- eglm(mpg ~ wt, family = gaussian, data = mtcars, reduce = F)
#
#   vcov_m1 <- with_seed(1813, sandwich::vcovBS(m1, cluster = NULL))
#   vcov_m2 <- with_seed(1813, sandwich::vcovBS(m2, cluster = NULL))
#   expect_equal(vcov_m2, vcov_m1)
#
#   vcov_m1 <- with_seed(1813, sandwich::vcovBS(m1, cluster = eval(m1$call$data)[, "cyl"]))
#   vcov_m2 <- with_seed(1813, sandwich::vcovBS(m2, cluster = eval(m2$call$data)[, "cyl"]))
#   expect_equal(vcov_m2, vcov_m1)
# })
