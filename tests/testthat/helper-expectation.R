expect_model_equal <- function(model2,model1) {
  expect_equal(model2$coefficients, model1$coefficients)
  expect_equal(model2$residuals, model1$residuals)
  expect_equal(model2$fitted.values, model1$fitted.values)
  expect_equal(model2$family$family, model1$family$family)
  expect_equal(model2$family$link, model1$family$link)
  expect_equal(model2$linear.predictors, model1$linear.predictors)
  expect_equal(model2$deviance, model1$deviance)
  expect_equal(model2$aic, model1$aic)
  expect_equal(model2$null.deviance, model1$null.deviance)
  expect_equal(model2$df.residual, model1$df.residual)
  expect_equal(model2$df.null, model1$df.null)
  expect_equal(model2$y, model1$y)
  expect_equal(model2$call$formula, model1$call$formula)
  expect_equal(model2$call$family, model1$call$family)
  expect_equal(model2$call$data, model1$call$data)
  expect_equal(model2$qr$tol, model1$qr$tol)

  expect_equal(model2$iter, model1$iter)
  expect_equal(model2$convergence, model1$convergence)

  # QR is different with reduction
  if (isFALSE(model2$reduce)) {
    expect_equal(model2$qr$qr, model1$qr$qr)
    expect_equal(model2$qr$rank, model1$qr$rank)
    expect_equal(model2$qr$qraux, model1$qr$qraux)
    expect_equal(model2$qr$pivot, model1$qr$pivot)
  }

  expect_equal(
    predict(model2, newdata = mtcars, type = "response"),
    predict(model1, newdata = mtcars, type = "response")
  )

  expect_equal(fitted(model2), fitted(model1))
}
