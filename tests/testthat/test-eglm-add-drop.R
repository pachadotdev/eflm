test_that("add/drop: eglm == glm, unweighted, no subset", {
  m <- "am ~ wt + mpg"

  m1 <- glm(m, data = mtcars)
  m2 <- eglm(m, data = mtcars, reduce = F)
  m3 <- eglm(m, data = mtcars, reduce = T)

  # no test
  expect_add_equal(add1(m2, ~ . + cyl), add1(m1, ~ . + cyl))
  expect_add_equal(add1(m3, ~ . + cyl), add1(m1, ~ . + cyl))
  expect_drop_equal(drop1(m2), drop1(m1))
  expect_drop_equal(drop1(m3), drop1(m1))

  test <- "F"
  expect_add_equal(add1(m2, ~ . + cyl, test = test), add1(m1, ~ . + cyl, test = test))
  expect_add_equal(add1(m3, ~ . + cyl, test = test), add1(m1, ~ . + cyl, test = test))
  expect_drop_equal(drop1(m2, test = test), drop1(m1, test = test))
  expect_drop_equal(drop1(m3, test = test), drop1(m1, test = test))

  test <- "Chisq"
  expect_add_equal(add1(m2, ~ . + cyl, test = test), add1(m1, ~ . + cyl, test = test))
  expect_add_equal(add1(m3, ~ . + cyl, test = test), add1(m1, ~ . + cyl, test = test))
  expect_drop_equal(drop1(m2, test = test), drop1(m1, test = test))
  expect_drop_equal(drop1(m3, test = test), drop1(m1, test = test))

  # test <- "LRT"
  expect_add_equal(add1(m2, ~ . + cyl, test = test), add1(m1, ~ . + cyl, test = test))
  expect_add_equal(add1(m3, ~ . + cyl, test = test), add1(m1, ~ . + cyl, test = test))
  expect_drop_equal(drop1(m2, test = test), drop1(m1, test = test))
  expect_drop_equal(drop1(m3, test = test), drop1(m1, test = test))

  # test <- "Rao"
  expect_add_equal(add1(m2, ~ . + cyl, test = test), add1(m1, ~ . + cyl, test = test))
  expect_add_equal(add1(m3, ~ . + cyl, test = test), add1(m1, ~ . + cyl, test = test))
  expect_drop_equal(drop1(m2, test = test), drop1(m1, test = test))
  expect_drop_equal(drop1(m3, test = test), drop1(m1, test = test))
})
