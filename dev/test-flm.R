fit_fglm <- fglm(mpg ~ wt, mtcars)
fit_flm <- flm(mpg ~ wt, mtcars)
fit_flm_2 <- lm(mpg ~ wt, mtcars)

fit_flm
fit_flm_2

summary(fit_flm)
summary(fit_flm_2)
