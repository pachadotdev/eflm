for (f in fmly) {
  test_that(sprintf("%s eglm summary is equivalent to glm", f), {
    m <- mdl(f)

    m1 <- summary(glm(m, family = f, data = mtcars))
    m2 <- summary(eglm(m, family = f, data = mtcars, reduce = F))
    m3 <- summary(eglm(m, family = f, data = mtcars, reduce = T))

    expect_summary_equal(m2, m1)
    expect_summary_equal(m3, m1)
  })
}
