expect_model_equal <- function(object, model_reference) {
  model <- quasi_label(rlang::enquo(object), arg = "object")

  expect(
    all.equal(model$val$coefficients, model_reference$coefficients),
    "the coefficients are not all equal"
  )
  expect(
    all.equal(coef(model$val), coef(model_reference)),
    "the coefficients are not all equal"
  )

  expect(
    all.equal(model$val$residuals, model_reference$residuals),
    "the residuals are not all equal"
  )

  expect(
    all.equal(model$val$fitted.values, model_reference$fitted.values),
    "the fitted.values are not all equal"
  )

  expect(
    all.equal(model$val$family$family, model_reference$family$family),
    "the family is not equal"
  )
  expect(
    all.equal(model$val$call$family, model_reference$call$family),
    "the family is not equal"
  )
  expect(
    all.equal(family(model$val), family(model_reference)),
    "the family is not equal"
  )

  expect(
    all.equal(model$val$family$link, model_reference$family$link),
    "the link is not equal"
  )

  expect(
    all.equal(model$val$linear.predictors, model_reference$linear.predictors),
    "the linear.predictors are not equal"
  )

  expect(
    all.equal(model$val$deviance, model_reference$deviance),
    "the deviance is not equal"
  )

  expect(
    all.equal(model$val$deviance, model_reference$deviance),
    "the deviance is not equal"
  )

  expect(
    all.equal(model$val$aic, model_reference$aic),
    "the aic is not equal"
  )

  expect(
    all.equal(model$val$null.deviance, model_reference$null.deviance),
    "the aic is not equal"
  )

  expect(
    all.equal(model$val$df.residual, model_reference$df.residual),
    "the df.residual is not equal"
  )

  expect(
    all.equal(model$val$df.null, model_reference$df.null),
    "the df.null is not equal"
  )

  expect(
    all.equal(model$val$y, model_reference$y),
    "the y's are not all equal"
  )

  expect(
    all.equal(model$val$call$formula, model_reference$call$formula),
    "the formula is not equal"
  )

  expect(
    all.equal(model$val$call$data, model_reference$call$data),
    "the data is not equal"
  )

  expect(
    all.equal(model$val$qr$tol, model_reference$qr$tol),
    "the qr tolerance is not equal"
  )

  expect(
    all.equal(model$val$iter, model_reference$iter),
    "the number of iterations is not equal"
  )

  expect(
    all.equal(model$val$converged, model_reference$converged),
    "the converged logical is not equal"
  )

  expect(
    all.equal(model$val$converged, model_reference$converged),
    "the converged logical is not equal"
  )

  # QR is different with reduction
  if (isFALSE(model$val$reduce)) {
    expect(
      all.equal(model$val$qr$qr, model_reference$qr$qr),
      "the qr matrix is not equal"
    )
    expect(
      all.equal(model$val$qr$rank, model_reference$qr$rank),
      "the qr rank is not equal"
    )
    expect(
      all.equal(model$val$qr$qraux, model_reference$qr$qraux),
      "the qraux vector is not equal"
    )
    expect(
      all.equal(model$val$qr$pivot, model_reference$qr$pivot),
      "the pivot vector is not equal"
    )
  }

  expect(
    all.equal(model$val$qr$pivot, model_reference$qr$pivot),
    "the pivot vector is not equal"
  )

  types <- if (!any(class(model$val) %in% "elm")) {
    c("link", "response", "terms")
  } else {
    c("response", "terms")
  }

  for (ty in types) {
    expect(
      all.equal(
        predict(model$val, newdata = mtcars, type = ty),
        predict(model_reference, newdata = mtcars, type = ty)
      ),
      "the model prediction is not equal"
    )
  }
  expect(
    all.equal(
      fitted(model$val),
      fitted(model_reference)
    ),
    "the model prediction is not equal"
  )

  invisible(model$val)
}
