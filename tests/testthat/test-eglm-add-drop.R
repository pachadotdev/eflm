# mdl <- function() {
#   if (!any(f %in% c("poisson", "binomial", "quasibinomial"))) {
#     "mpg ~ wt + am"
#   } else {
#     "am ~ wt + mpg"
#   }
# }
#
# for (f in fmly) {
#   test_that(sprintf("add/drop: eglm == glm, %s link, unweighted, no subset", f), {
#     m1 <- glm(mpg ~ wt, data = mtcars, family = gaussian)
#     m2 <- eglm(mpg ~ wt, data = mtcars, family = f, reduce = FALSE)
#     m3 <- eglm(mpg ~ wt, data = mtcars, family = f, reduce = TRUE)
#
#     # no test
#     # expect_add_equal(add1(m2, ~ . + hp), add1(m1, ~ . + hp))
#     # expect_add_equal(add1(m3, ~ . + hp), add1(m1, ~ . + hp))
#     # expect_drop_equal(drop1(m2), drop1(m1))
#     # expect_drop_equal(drop1(m3), drop1(m1))
#
#     # test <- "F"
#     # expect_add_equal(add1(m2, ~ . + hp, test = test), add1(m1, ~ . + hp, test = test))
#     # expect_add_equal(add1(m3, ~ . + hp, test = test), add1(m1, ~ . + hp, test = test))
#     # expect_drop_equal(drop1(m2, test = test), drop1(m1, test = test))
#     # expect_drop_equal(drop1(m3, test = test), drop1(m1, test = test))
#     #
#     # test <- "Chisq"
#     # expect_add_equal(add1(m2, ~ . + hp, test = test), add1(m1, ~ . + hp, test = test))
#     # expect_add_equal(add1(m3, ~ . + hp, test = test), add1(m1, ~ . + hp, test = test))
#     # expect_drop_equal(drop1(m2, test = test), drop1(m1, test = test))
#     # expect_drop_equal(drop1(m3, test = test), drop1(m1, test = test))
#
#     # test <- "LRT"
#     # expect_add_equal(add1(m2, ~ . + hp, test = test), add1(m1, ~ . + hp, test = test))
#     # expect_add_equal(add1(m3, ~ . + hp, test = test), add1(m1, ~ . + hp, test = test))
#     # expect_drop_equal(drop1(m2, test = test), drop1(m1, test = test))
#     # expect_drop_equal(drop1(m3, test = test), drop1(m1, test = test))
#
#     # test <- "Rao"
#     # expect_add_equal(add1(m2, ~ . + hp, test = test), add1(m1, ~ . + hp, test = test))
#     # expect_add_equal(add1(m3, ~ . + hp, test = test), add1(m1, ~ . + hp, test = test))
#     # expect_drop_equal(drop1(m2, test = test), drop1(m1, test = test))
#     # expect_drop_equal(drop1(m3, test = test), drop1(m1, test = test))
#   })
# }
