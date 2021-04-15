# NEVER PUT THIS ON TESTS/ FOLDER
# THE DATA IS TOO LARGE AND TAKES +5 MINUTES TO FIT THE MODEL

library(dplyr)
library(microbenchmark)

m1 <- glm(trade ~ log_dist + cntg + lang + clny + exp_year + imp_year,
                family = quasipoisson(link = "log"),
                data = trade_demo_glm,
                y = FALSE,
                model = FALSE
)

m2 <- eglm(trade ~ log_dist + cntg + lang + clny + exp_year + imp_year,
                family = quasipoisson(link = "log"),
                data = trade_demo_glm,
                y = FALSE,
                model = FALSE
)

expect_equal(m1$coefficients, m2$coefficients)
expect_equal(m1$residuals, m2$residuals)
expect_equal(m1$fitted.values, m2$fitted.values)
expect_equal(m1$family$family, m2$family$family)
expect_equal(m1$family$link, m2$family$link)
expect_equal(m1$linear.predictors, m2$linear.predictors)
expect_equal(m1$deviance, m2$deviance)
expect_equal(m1$aic, m2$aic)
expect_equal(m1$null.deviance, m2$null.deviance)
expect_equal(m1$df.residual, m2$df.residual)
expect_equal(m1$df.null, m2$df.null)
expect_equal(m1$y, m2$y)
expect_equal(m1$call$formula, m2$call$formula)
expect_equal(m1$call$family, m2$call$family)
expect_equal(m1$call$data, m2$call$data)

expect_warning(expect_equal(
  predict(m1, newdata = ch1_application1_2, type = "link"),
  predict(m2, newdata = ch1_application1_2, type = "link")
))

expect_warning(expect_equal(
  predict(m1, newdata = ch1_application1_2, type = "response"),
  predict(m2, newdata = ch1_application1_2, type = "response")
))

microbenchmark(
  lm(trade ~ log_dist + cntg + lang + clny + exp_year + imp_year,
      data = trade_demo_glm,
      y = FALSE,
      model = FALSE
  ),
  flm(trade ~ log_dist + cntg + lang + clny + exp_year + imp_year,
       data = trade_demo_glm,
       y = FALSE,
       model = FALSE
  ),
  times = 10L
)

# Unit: seconds
# expr
# lm(trade ~ log_dist + cntg + lang + clny + exp_year + imp_year,      data = ch1_application1_2, y = FALSE, model = FALSE)
# flm(trade ~ log_dist + cntg + lang + clny + exp_year + imp_year,      data = ch1_application1_2, y = FALSE, model = FALSE)
# min        lq      mean    median        uq      max neval
# 12.001917 14.563548 16.028608 17.150659 17.791202 18.36267    10
# 2.267154  2.329043  2.833997  2.664455  3.291076  3.64414    10
