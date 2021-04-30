# Fitting ----

for (f in fmly) {
  test_that(sprintf("fitting: eglm == glm, %s link, unweighted, no subset", f), {
    m <- mdl(f)

    m1 <- glm(m, family = f, data = mtcars)
    m2 <- eglm(m, family = f, data = mtcars, reduce = F)
    m3 <- eglm(m, family = f, data = mtcars, reduce = T)

    expect_model_equal(m2,m1)
    expect_model_equal(m3,m1)
  })
}

for (f in fmly) {
  test_that(sprintf("fitting: eglm == glm, %s link, weighted, no subset", f), {
    m <- mdl(f)

    m1 <- glm(m, family = f, data = mtcars, weights = mtcars$cyl)
    m2 <- eglm(m, family = f, data = mtcars, weights = mtcars$cyl, reduce = F)
    m3 <- eglm(m, family = f, data = mtcars, weights = mtcars$cyl, reduce = T)

    expect_model_equal(m2,m1)
    expect_model_equal(m3,m1)
  })
}

for (f in fmly) {
  test_that(sprintf("fitting: eglm == glm, %s link, unweighted, subset", f), {
    m <- mdl(f)

    m1 <- glm(m, family = f, data = mtcars, subset = (cyl < 8))
    m2 <- eglm(m, family = f, data = mtcars, subset = (cyl < 8), reduce = F)
    m3 <- eglm(m, family = f, data = mtcars, subset = (cyl < 8), reduce = T)

    expect_model_equal(m2,m1)
    expect_model_equal(m3,m1)
  })
}

for (f in fmly) {
  test_that(sprintf("fitting: eglm == glm, %s link, weighted, subset", f), {
    m <- mdl(f)

    m1 <- glm(m, family = f, data = mtcars, weights = mtcars$cyl, subset = (cyl < 8))
    m2 <- eglm(m, family = f, data = mtcars, weights = mtcars$cyl, subset = (cyl < 8), reduce = F)
    m3 <- eglm(m, family = f, data = mtcars, weights = mtcars$cyl, subset = (cyl < 8), reduce = T)

    expect_model_equal(m2,m1)
    expect_model_equal(m3,m1)
  })
}

# Convergence ----

for (f in fmly) {
  test_that(sprintf("convergence: eglm == glm, %s link", f), {
    m <- mdl(f)

    m1 <- glm(am ~ mpg + wt, family = binomial, data = mtcars, weights = mtcars$cyl)
    m2 <- eglm(am ~ mpg + wt, family = binomial, data = mtcars, weights = mtcars$cyl, reduce = F)
    m3 <- eglm(am ~ mpg + wt, family = binomial, data = mtcars, weights = mtcars$cyl, reduce = T)

    expect_equal(m2$iter, m1$iter)
    expect_equal(m2$convergence, m1$convergence)

    expect_equal(m3$iter, m1$iter)
    expect_equal(m3$convergence, m1$convergence)
  })
}

test_that("convergence: eglm == glm, logit fails to converge with a large number of variables", {
  # Warning messages:
  #  1: glm.fit: algorithm did not converge
  #  2: glm.fit: fitted probabilities numerically 0 or 1 occurred
  expect_warning(
    expect_warning(glm(am ~ ., family = binomial, data = mtcars))
  )

  # Warning messages:
  #  1: eglm.wfit: algorithm did not converge
  #  2: eglm.wfit: fitted probabilities numerically 0 or 1 occurred
  expect_warning(
    expect_warning(eglm(am ~ ., family = binomial, data = mtcars, reduce = F))
  )

  expect_warning(
    expect_warning(eglm(am ~ ., family = binomial, data = mtcars, reduce = T))
  )
})

# Design matrix ----

test_that("design matrix: eglm == glm", {
  m1 <- glm(mpg ~ wt, data = mtcars, x = TRUE)
  m2 <- eglm(mpg ~ wt, data = mtcars, x = TRUE, reduce = FALSE)
  m3 <- eglm(mpg ~ wt, data = mtcars, x = TRUE, reduce = TRUE)

  expect_equal(m2$x, m1$x)
  expect_equal(m3$x, m1$x)
})
