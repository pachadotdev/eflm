for (f in fmly) {
  test_that(sprintf("%s eglm summary is equivalent to glm", f), {
    m1 <- summary(glm(mpg ~ wt, family = gaussian, data = mtcars))
    m2 <- summary(eglm(mpg ~ wt, family = gaussian, data = mtcars, reduce = F))
    m3 <- summary(eglm(mpg ~ wt, family = gaussian, data = mtcars, reduce = T))

    expect_summary_equal(m2, m1)
    expect_summary_equal(m3, m1)
  })
}
