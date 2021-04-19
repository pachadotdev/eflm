# NEVER PUT THIS ON TESTS/ FOLDER
# THE DATA IS TOO LARGE AND TAKES +5 MINUTES TO FIT THE MODEL

library(eflm)

trade_data_yotov <- readRDS("dev/trade_data_yotov.rds")

model <- "trade ~ log(dist) + cntg + lang + clny + exp_year + imp_year"

m1 <- glm(model,
          family = quasipoisson(link = "log"),
          data = trade_data_yotov,
          y = FALSE,
          model = FALSE
)

m2 <- eglm(model,
           family = quasipoisson(link = "log"),
           data = trade_data_yotov,
           y = FALSE,
           model = FALSE,
           reduce = TRUE
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

# Below I expected:
# Warning messages:
# 1: In predict.lm(object, newdata, se.fit, scale = 1, type = if (type ==  :
#  prediction from a rank-deficient fit may be misleading
# 2: In predict.lm(object, newdata, se.fit, scale = 1, type = if (type ==  :
#  prediction from a rank-deficient fit may be misleading
# which is not strange at all

expect_equal(
  predict(m1, newdata = trade_data_yotov, type = "link"),
  predict(m2, newdata = trade_data_yotov, type = "link")
)

expect_equal(
  predict(m1, newdata = trade_data_yotov, type = "response"),
  predict(m2, newdata = trade_data_yotov, type = "response")
)
