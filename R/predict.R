#' Predict Method for FGLM Fits
#'
#' Obtains predictions and optionally estimates standard errors of
#' those predictions from a fitted generalized linear model object (see the
#' function \link{predict.glm}).
#'
#' @param object a fitted object of class inheriting from "fglm"
#' @param newdata optionally, a data frame in which to look for variables with
#' which to predict. If omitted, the fitted linear predictors are used
#' @param type the type of prediction required. The default is on the scale of
#' the linear predictors; the alternative "response" is on the scale of the
#' response variable. Thus for a default binomial model the default predictions
#' are of log-odds (probabilities on logit scale) and type = "response" gives
#' the predicted probabilities. The "terms" option returns a matrix giving the
#' fitted values of each term in the model formula on the linear predictor
#' scale
#' @param se.fit A switch indicating if standard errors are required.
#' @param na.action function determining what should be done with missing values
#' in newdata. The default is to predict NA
#' @param ... further arguments passed to or from other methods
#' @importFrom stats family fitted .checkMFClasses
#' @export
#' @keywords internal
predict.fglm <- function(object, newdata, se.fit = FALSE, scale = NULL, df = Inf,
                         interval = c("none", "confidence", "prediction"), level = 0.95,
                         type = c("link", "response", "terms"), terms = NULL, na.action = na.pass,
                         pred.var = res.var/weights, weights = 1, ...) {
  type <- match.arg(type)
  if (missing(newdata) & is.null(object$linear.predictors)) {
    warning("Fitted values were not returned from the fglm object:
            use the original data by setting argument 'newdata' or refit
            the model by specifying fitted=TRUE.")
  }
  na.act <- object$na.action
  object$na.action <- NULL
  if (missing(newdata)) {
    pred <- switch(type,
      link = object$linear.predictors,
      response = fitted(object)
    )
    if (!is.null(na.act)) pred <- napredict(na.act, pred)
  } else {
    pred <- predict.flm(object, newdata,
      type = "response",
      na.action = na.action
    )
    switch(type, response = {
      pred <- stats::family(object)$linkinv(pred)
    }, link = )
  }
  pred
}

#' @export
#' @importFrom stats napredict delete.response
#' @keywords internal
predict.flm <- function(object, newdata, se.fit = FALSE, scale = NULL, df = Inf,
                        interval = c("none", "confidence", "prediction"), level = 0.95,
                        type = c("response", "terms"), terms = NULL, na.action = na.pass,
                        pred.var = res.var/weights, weights = 1, ...) {
  tt <- terms(object)
  if (!inherits(object, c("flm", "fglm"))) {
    warning("calling predict.flm(<fake-flm/fglm-object>) ...")
  }
  if (missing(newdata) || is.null(newdata)) {
    if (is.null(object$fitted.values)) {
      warning("Fitted values were not returned from the fglm object:
              use the original data by setting argument 'newdata' or refit
              the model by specifying fitted=TRUE.")
    }
    return(object$fitted.values)
  } else {
    Terms <- delete.response(tt)
    m <- model.frame(Terms, newdata, na.action = na.action) # , xlev = object$xlevels)
    if (!is.null(cl <- attr(Terms, "dataClasses"))) {
      .checkMFClasses(cl, m)
    }
    X <- model.matrix(Terms, m, contrasts.arg = object$contrasts)
    offset <- rep(0, nrow(X))
    if (!is.null(off.num <- attr(tt, "offset"))) {
      for (i in off.num) {
        offset <- offset + eval(attr(
          tt,
          "variables"
        )[[i + 1]], newdata)
      }
    }
    if (!is.null(object$call$offset)) {
      offset <- offset + eval(object$call$offset, newdata)
    }
  }
  p <- object$rank
  ord <- colnames(X)
  if (p < ncol(X) && !(missing(newdata) || is.null(newdata))) {
    warning("Prediction from a rank-deficient fit may be misleading.")
  }
  beta <- object$coefficients
  beta[is.na(beta)] <- 0
  predictor <- drop(X[, ord, drop = FALSE] %*% beta[ord])
  if (!is.null(offset)) {
    predictor <- predictor + offset
  }
  if (missing(newdata) && !is.null(na.act <- object$na.action)) {
    predictor <- napredict(na.act, predictor)
  }
  if (se.fit || interval != "none") {
    w <- object$weights
    res.var <- if (is.null(scale)) {
      r <- object$residuals
      rss <- sum(if (is.null(w)) r^2 else r^2 * w)
      df <- object$df.residual
      rss/df
    } else {
      scale^2
    }
    if (type != "terms") {
      p <- object$rank
      p1 <- seq_len(p)
      piv <- if (p) object$qr$pivot[p1]
      X <- model.matrix(object)
      if (p > 0) {
        XRinv <- if (missing(newdata) && is.null(w)) {
          qr.Q(object$qr)[, p1, drop = FALSE]
        } else {
          X[, piv] %*% qr.solve(qr.R(object$qr)[p1,p1])
        }
        ip <- drop(XRinv^2 %*% rep(res.var, p))
      } else {
        ip <- rep(0, n)
      }
    }
  }
  if (se.fit || interval != "none") {
    se <- sqrt(ip)
    if (type == "terms" && !is.null(terms) && !se.fit)
      se <- se[, terms, drop = FALSE]
  }
  if (missing(newdata) && !is.null(na.act <- object$na.action)) {
    predictor <- napredict(na.act, predictor)
    if (se.fit)
      se <- napredict(na.act, se)
  }
  predictor
}
