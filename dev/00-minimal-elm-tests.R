x = cbind(matrix(rep(1,length(mtcars$wt)), ncol = 1), mtcars$wt)
dim(x)
class(x)

y = matrix(mtcars$mpg, ncol = 1)

w = rep(1,length(mtcars$mpg))

elm.wfit(x, y, w, reduce = F)
elm.wfit(x, y, w, reduce = T)

eglm.wfit(x, y, w)
