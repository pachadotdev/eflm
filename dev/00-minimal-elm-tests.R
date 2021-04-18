x = cbind(matrix(rep(1,length(mtcars$wt)), ncol = 1), mtcars$wt)
dim(x)
class(x)

y = matrix(mtcars$mpg, ncol = 1)

weights = rep(1,length(mtcars$mpg))

lm1 <- elm.wfit(x, y, weights, reduce = F)
lm2 <- elm.wfit(x, y, weights, reduce = T)

glm1 <- eglm.wfit(x, y, weights, reduce = F)
glm2 <- eglm.wfit(x, y, weights, reduce = T)

glm12 <- eglm(mpg ~ wt, data = mtcars, reduce = F, x = T)
glm22 <- eglm(mpg ~ wt, data = mtcars, reduce = T, x = T)
