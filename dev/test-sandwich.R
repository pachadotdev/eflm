library(sandwich)

x = fglm(mpg ~ wt + cyl, mtcars)
y = glm(mpg ~ wt + cyl, mtcars)

estfun(x)
estfun(y)

vcov_x <- sandwich::vcovCL(
  x,
  cluster = mtcars$cyl
)

vcov_y <- sandwich::vcovCL(
  y,
  cluster = mtcars$cyl
)
