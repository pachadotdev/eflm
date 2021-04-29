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
    all.equal(vcov(model$val), vcov(model_reference)),
    "the variance-covariance matrix is not equal"
  )

  expect(
    all.equal(model$val$residuals, model_reference$residuals),
    "the residuals are not all equal"
  )

  expect(
    all.equal(model$val$fitted.values, model_reference$fitted.values),
    "the fitted values are not all equal"
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
    "the linear predictors are not equal"
  )

  expect(
    all.equal(model$val$deviance, model_reference$deviance),
    "the deviance is not equal"
  )
  expect(
    all.equal(deviance(model$val), deviance(model_reference)),
    "the deviance is not equal"
  )

  expect(
    all.equal(model$val$aic, model_reference$aic),
    "the aic is not equal"
  )

  expect(
    all.equal(model$val$null.deviance, model_reference$null.deviance),
    "the null deviance is not equal"
  )

  expect(
    all.equal(model$val$df.residual, model_reference$df.residual),
    "the residual degrees of freedom are not equal"
  )

  expect(
    all.equal(model$val$df.null, model_reference$df.null),
    "the null degrees of freedom are not equal"
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

  expect(
    all.equal(nobs(model$val), nobs(model_reference)),
    "the number of observations is not equal"
  )

  expect(
    all.equal(model.matrix(model$val), model.matrix(model_reference)),
    "the model matrix is not equal"
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
      "the qraux is not equal"
    )
    expect(
      all.equal(model$val$qr$pivot, model_reference$qr$pivot),
      "the pivot is not equal"
    )
  }

  expect(
    all.equal(model$val$qr$pivot, model_reference$qr$pivot),
    "the pivot is not equal"
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
      "the prediction is not equal"
    )
  }
  expect(
    all.equal(
      fitted(model$val),
      fitted(model_reference)
    ),
    "the prediction is not equal"
  )

  invisible(model$val)
}

expect_summary_equal <- function(object, model_summary_reference) {
  model_summary <- quasi_label(rlang::enquo(object), arg = "object")

  expect(
    all.equal(model_summary$val$coefficients, model_summary_reference$coefficients),
    "the coefficients, std errors, t values and p values are not all equal"
  )

  expect(
    all.equal(model_summary$val$call$formula, model_summary_reference$call$formula),
    "the formula is not equal"
  )

  expect(
    all.equal(model_summary$val$call$family, model_summary_reference$call$family),
    "the family is not equal"
  )

  expect(
    all.equal(model_summary$val$call$data, model_summary_reference$call$data),
    "the data is not equal"
  )

  expect(
    all.equal(model_summary$val$terms, model_summary_reference$terms),
    "the terms are not all equal"
  )

  expect(
    all.equal(model_summary$val$deviance, model_summary_reference$deviance),
    "the deviance is not equal"
  )
  expect(
    all.equal(deviance(model_summary$val), deviance(model_summary_reference)),
    "the deviance is not equal"
  )

  expect(
    all.equal(model_summary$val$aic, model_summary_reference$aic),
    "the AIC is not equal"
  )

  expect(
    all.equal(model_summary$val$df.residual, model_summary_reference$df.residual),
    "the residual degrees of freedom are not equal"
  )

  expect(
    all.equal(model_summary$val$df.null, model_summary_reference$df.null),
    "the null degrees of freedom are not equal"
  )

  expect(
    all.equal(model_summary$val$null.deviance, model_summary_reference$null.deviance),
    "the null deviance is not equal"
  )

  expect(
    all.equal(model_summary$val$iter, model_summary_reference$iter),
    "the number of iterations is not equal"
  )

  expect(
    all.equal(model_summary$val$deviance.resid, model_summary_reference$deviance.resid),
    "the residual deviance is not equal"
  )

  expect(
    all.equal(model_summary$val$dispersion, model_summary_reference$dispersion),
    "the dispersion parameter is not equal"
  )

  expect(
    all.equal(model_summary$val$cov.unscaled, model_summary_reference$cov.unscaled),
    "the unscaled covariance is not equal"
  )

  expect(
    all.equal(model_summary$val$cov.scaled, model_summary_reference$cov.scaled),
    "the scaled covariance is not equal"
  )

  expect(
    all.equal(model_summary$val$aliased, model_summary_reference$aliased),
    "the alised logicals are not all equal"
  )

  expect(
    all.equal(model_summary$val$sigma, model_summary_reference$sigma),
    "the sigma is not equal"
  )

  expect(
    all.equal(model_summary$val$df, model_summary_reference$df),
    "the degrees of freedom are not equal"
  )

  expect(
    all.equal(model_summary$val$r.squared, model_summary_reference$r.squared),
    "the R squared is not equal"
  )

  expect(
    all.equal(model_summary$val$adj.r.squared, model_summary_reference$adj.r.squared),
    "the adjusted R squared is not equal"
  )

  expect(
    all.equal(model_summary$val$fstatistic, model_summary_reference$fstatistic),
    "the F statistic is not equal"
  )

  invisible(model_summary$val)
}

expect_add_equal <- function(object, add_reference) {
  add <- quasi_label(rlang::enquo(object), arg = "object")

  expect(
    all.equal(add$val$Df, add_reference$Df),
    "the add degrees of freedom are not equal"
  )
  expect(
    all.equal(add$val$Deviance, add_reference$Deviance),
    "the add deviance is not equal"
  )
  expect(
    all.equal(add$val$AIC, add_reference$AIC),
    "the add AIC is not equal"
  )

  invisible(add$val)
}

expect_drop_equal <- function(object, drop_reference) {
  drop <- quasi_label(rlang::enquo(object), arg = "object")

  expect(
    all.equal(drop$val$Df, drop_reference$Df),
    "the drop degrees of freedom are not equal"
  )
  expect(
    all.equal(drop$val$Deviance, drop_reference$Deviance),
    "the drop deviance is not equal"
  )
  expect(
    all.equal(drop$val$AIC, drop_reference$AIC),
    "the drop AIC is not equal"
  )

  invisible(drop$val)
}
