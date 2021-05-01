expect_model_equal <- function(object, reference) {
  model <- testthat::quasi_label(rlang::enquo(object), arg = "object")
  reference <- testthat::quasi_label(rlang::enquo(reference), arg = "expected")

  testthat::expect(
    all.equal(model$val$coefficients, reference$val$coefficients),
    "the coefficients are not all equal"
  )
  testthat::expect(
    all.equal(coef(model$val), coef(reference$val)),
    "the coefficients are not all equal"
  )

  testthat::expect(
    all.equal(vcov(model$val), vcov(reference$val)),
    "the variance-covariance matrix is not equal"
  )

  testthat::expect(
    all.equal(model$val$residuals, reference$val$residuals),
    "the residuals are not all equal"
  )

  testthat::expect(
    all.equal(model$val$fitted.values, reference$val$fitted.values),
    "the fitted values are not all equal"
  )

  testthat::expect(
    all.equal(model$val$family$family, reference$val$family$family),
    "the family is not equal"
  )
  testthat::expect(
    all.equal(model$val$call$family, reference$val$call$family),
    "the family is not equal"
  )
  testthat::expect(
    all.equal(family(model$val), family(reference$val)),
    "the family is not equal"
  )

  testthat::expect(
    all.equal(model$val$family$link, reference$val$family$link),
    "the link is not equal"
  )

  testthat::expect(
    all.equal(model$val$linear.predictors, reference$val$linear.predictors),
    "the linear predictors are not equal"
  )

  testthat::expect(
    all.equal(model$val$deviance, reference$val$deviance),
    "the deviance is not equal"
  )
  testthat::expect(
    all.equal(deviance(model$val), deviance(reference$val)),
    "the deviance is not equal"
  )

  testthat::expect(
    all.equal(model$val$aic, reference$val$aic),
    "the aic is not equal"
  )

  testthat::expect(
    all.equal(model$val$null.deviance, reference$val$null.deviance),
    "the null deviance is not equal"
  )

  testthat::expect(
    all.equal(model$val$df.residual, reference$val$df.residual),
    "the residual degrees of freedom are not equal"
  )

  testthat::expect(
    all.equal(model$val$df.null, reference$val$df.null),
    "the null degrees of freedom are not equal"
  )

  if (!is.null(model$val$x)) {
    testthat::expect(
      all.equal(model$val$x, reference$val$x),
      "the design matrix is not equal"
    )
  }

  testthat::expect(
    all.equal(model$val$y, reference$val$y),
    "the y's are not all equal"
  )

  testthat::expect(
    all.equal(model$val$call$formula, reference$val$call$formula),
    "the formula is not equal"
  )

  testthat::expect(
    all.equal(model$val$call$data, reference$val$call$data),
    "the data is not equal"
  )

  testthat::expect(
    all.equal(model$val$qr$tol, reference$val$qr$tol),
    "the qr tolerance is not equal"
  )

  testthat::expect(
    all.equal(model$val$iter, reference$val$iter),
    "the number of iterations is not equal"
  )

  testthat::expect(
    all.equal(model$val$converged, reference$val$converged),
    "the converged logical is not equal"
  )

  testthat::expect(
    all.equal(model$val$converged, reference$val$converged),
    "the converged logical is not equal"
  )

  testthat::expect(
    all.equal(nobs(model$val), nobs(reference$val)),
    "the number of observations is not equal"
  )

  testthat::expect(
    all.equal(model.matrix(model$val), model.matrix(reference$val)),
    "the model matrix is not equal"
  )

  # QR is different with reduction
  if (isFALSE(model$val$reduce)) {
    testthat::expect(
      all.equal(model$val$qr$qr, reference$val$qr$qr),
      "the qr matrix is not equal"
    )
    testthat::expect(
      all.equal(model$val$qr$rank, reference$val$qr$rank),
      "the qr rank is not equal"
    )
    testthat::expect(
      all.equal(model$val$qr$qraux, reference$val$qr$qraux),
      "the qraux is not equal"
    )
    testthat::expect(
      all.equal(model$val$qr$pivot, reference$val$qr$pivot),
      "the pivot is not equal"
    )
  }

  testthat::expect(
    all.equal(model$val$qr$pivot, reference$val$qr$pivot),
    "the pivot is not equal"
  )

  types <- if (!any(class(model$val) %in% "elm")) {
    c("link", "response", "terms")
  } else {
    c("response", "terms")
  }

  for (ty in types) {
    testthat::expect(
      all.equal(
        predict(model$val, newdata = mtcars, type = ty),
        predict(reference$val, newdata = mtcars, type = ty)
      ),
      "the prediction is not equal"
    )
  }
  testthat::expect(
    all.equal(
      fitted(model$val),
      fitted(reference$val)
    ),
    "the prediction is not equal"
  )

  invisible(model$val)
}

expect_summary_equal <- function(object, reference) {
  model_summary <- testthat::quasi_label(rlang::enquo(object), arg = "object")
  reference <- testthat::quasi_label(rlang::enquo(reference), arg = "expected")

  testthat::expect(
    all.equal(model_summary$val$coefficients, reference$val$coefficients),
    "the coefficients, std errors, t values and p values are not all equal"
  )

  testthat::expect(
    all.equal(model_summary$val$call$formula, reference$val$call$formula),
    "the formula is not equal"
  )

  testthat::expect(
    all.equal(model_summary$val$call$family, reference$val$call$family),
    "the family is not equal"
  )

  testthat::expect(
    all.equal(model_summary$val$call$data, reference$val$call$data),
    "the data is not equal"
  )

  testthat::expect(
    all.equal(model_summary$val$terms, reference$val$terms),
    "the terms are not all equal"
  )

  testthat::expect(
    all.equal(model_summary$val$deviance, reference$val$deviance),
    "the deviance is not equal"
  )
  testthat::expect(
    all.equal(deviance(model_summary$val), deviance(reference$val)),
    "the deviance is not equal"
  )

  testthat::expect(
    all.equal(model_summary$val$aic, reference$val$aic),
    "the AIC is not equal"
  )

  testthat::expect(
    all.equal(model_summary$val$df.residual, reference$val$df.residual),
    "the residual degrees of freedom are not equal"
  )

  testthat::expect(
    all.equal(model_summary$val$df.null, reference$val$df.null),
    "the null degrees of freedom are not equal"
  )

  testthat::expect(
    all.equal(model_summary$val$null.deviance, reference$val$null.deviance),
    "the null deviance is not equal"
  )

  testthat::expect(
    all.equal(model_summary$val$iter, reference$val$iter),
    "the number of iterations is not equal"
  )

  testthat::expect(
    all.equal(model_summary$val$deviance.resid, reference$val$deviance.resid),
    "the residual deviance is not equal"
  )

  testthat::expect(
    all.equal(model_summary$val$dispersion, reference$val$dispersion),
    "the dispersion parameter is not equal"
  )

  testthat::expect(
    all.equal(model_summary$val$cov.unscaled, reference$val$cov.unscaled),
    "the unscaled covariance is not equal"
  )

  testthat::expect(
    all.equal(model_summary$val$cov.scaled, reference$val$cov.scaled),
    "the scaled covariance is not equal"
  )

  testthat::expect(
    all.equal(model_summary$val$aliased, reference$val$aliased),
    "the alised logicals are not all equal"
  )

  testthat::expect(
    all.equal(model_summary$val$sigma, reference$val$sigma),
    "the sigma is not equal"
  )

  testthat::expect(
    all.equal(model_summary$val$df, reference$val$df),
    "the degrees of freedom are not equal"
  )

  testthat::expect(
    all.equal(model_summary$val$r.squared, reference$val$r.squared),
    "the R squared is not equal"
  )

  testthat::expect(
    all.equal(model_summary$val$adj.r.squared, reference$val$adj.r.squared),
    "the adjusted R squared is not equal"
  )

  testthat::expect(
    all.equal(model_summary$val$fstatistic, reference$val$fstatistic),
    "the F statistic is not equal"
  )

  invisible(model_summary$val)
}

expect_add_equal <- function(object, add_reference) {
  add <- quasi_label(rlang::enquo(object), arg = "object")

  testthat::expect(
    all.equal(add$val$Df, add_reference$val$Df),
    "the add degrees of freedom are not equal"
  )
  testthat::expect(
    all.equal(add$val$Deviance, add_reference$val$Deviance),
    "the add deviance is not equal"
  )
  testthat::expect(
    all.equal(add$val$AIC, add_reference$val$AIC),
    "the add AIC is not equal"
  )

  if (any("F value" %in% add$val)) {
    testthat::expect(
      all.equal(add$val$`F value`, add_reference$val$`F value`),
      "the add F value is not equal"
    )
    testthat::expect(
      all.equal(add$val$`Pr(>F)`, add_reference$val$`Pr(>F)`),
      "the add F value is not equal"
    )
  }

  if (any("scaled dev." %in% add$val)) {
    testthat::expect(
      all.equal(add$val$`scaled dev.`, add_reference$val$`scaled dev.`),
      "the add F value is not equal"
    )
    testthat::expect(
      all.equal(add$val$`Pr(>Chi)`, add_reference$val$`Pr(>Chi)`),
      "the add F value is not equal"
    )
  }

  if (any("scaled Rao dev." %in% add$val)) {
    testthat::expect(
      all.equal(add$val$`scaled Rao dev.`, add_reference$val$`scaled Rao dev.`),
      "the add F value is not equal"
    )
    testthat::expect(
      all.equal(add$val$`Pr(>Chi)`, add_reference$val$`Pr(>Chi)`),
      "the add F value is not equal"
    )
  }

  invisible(add$val)
}

expect_drop_equal <- function(object, drop_reference) {
  drop <- quasi_label(rlang::enquo(object), arg = "object")

  testthat::expect(
    all.equal(drop$val$Df, drop_reference$val$Df),
    "the drop degrees of freedom are not equal"
  )
  testthat::expect(
    all.equal(drop$val$Deviance, drop_reference$val$Deviance),
    "the drop deviance is not equal"
  )
  testthat::expect(
    all.equal(drop$val$AIC, drop_reference$val$AIC),
    "the drop AIC is not equal"
  )

  if (any("F value" %in% drop$val)) {
    testthat::expect(
      all.equal(drop$val$`F value`, add_reference$val$`F value`),
      "the drop F value is not equal"
    )
    testthat::expect(
      all.equal(drop$val$`Pr(>F)`, add_reference$val$`Pr(>F)`),
      "the drop F value is not equal"
    )
  }

  if (any("scaled dev." %in% drop$val)) {
    testthat::expect(
      all.equal(drop$val$`scaled dev.`, add_reference$val$`scaled dev.`),
      "the drop F value is not equal"
    )
    testthat::expect(
      all.equal(drop$val$`Pr(>Chi)`, add_reference$val$`Pr(>Chi)`),
      "the drop F value is not equal"
    )
  }

  if (any("scaled Rao dev." %in% drop$val)) {
    testthat::expect(
      all.equal(drop$val$`scaled Rao dev.`, add_reference$val$`scaled Rao dev.`),
      "the drop F value is not equal"
    )
    testthat::expect(
      all.equal(drop$val$`Pr(>Chi)`, add_reference$val$`Pr(>Chi)`),
      "the drop F value is not equal"
    )
  }

  invisible(drop$val)
}
