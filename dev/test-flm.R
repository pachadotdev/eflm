fit_fglm <- fglm(mpg ~ wt, mtcars)
fit_glm <- glm(mpg ~ wt, data = mtcars)

fit_flm <- flm(mpg ~ wt, mtcars)
fit_lm <- lm(mpg ~ wt, mtcars)

x <- summary(fit_flm)
y <- summary(fit_lm)

names(fit_glm)
names(fit_fglm)
