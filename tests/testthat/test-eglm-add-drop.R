# test_that("add/drop: eglm == glm, unweighted, no subset", {
#   m <- "am ~ wt + mpg"
#
#   m1 <- glm(m, data = mtcars)
#   m2 <- eglm(m, data = mtcars, reduce = F)
#   m3 <- eglm(m, data = mtcars, reduce = T)
#
#   for (t in tst) {
#     expect_add_equal(add1(m2, ~ . + cyl, test = t), add1(m1, ~ . + cyl, test = t))
#     expect_add_equal(add1(m3, ~ . + cyl, test = t), add1(m1, ~ . + cyl, test = t))
#     expect_drop_equal(drop1(m2, test = t), drop1(m1, test = t))
#     expect_drop_equal(drop1(m3, test = t), drop1(m1, test = t))
#   }
# })
