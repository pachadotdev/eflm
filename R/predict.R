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
#' @param na.action function determining what should be done with missing values
#' in newdata. The default is to predict NA
#' @param ... further arguments passed to or from other methods
#' @export

predict.fglm <- function(object, newdata = NULL, type = c("link", "response"),
                             na.action = na.pass, ...) {
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
      pred <- family(object)$linkinv(pred)
    }, link = )
  }
  pred
}

predict.flm <- function (object, newdata, na.action = na.pass, ...) {
  tt <- terms(object)
  if (!inherits(object, c("flm","fglm")))
    warning("calling predict.flm(<fake-flm/fglm-object>) ...")
  if (missing(newdata) || is.null(newdata)) {
    if(is.null(object$fitted.values))
      warning("Fitted values were not returned from the fglm object:
              use the original data by setting argument 'newdata' or refit
              the model by specifying fitted=TRUE.")
    return(object$fitted.values)
  }
  else {
    Terms <- delete.response(tt)
    m <- model.frame(Terms, newdata, na.action = na.action)#, xlev = object$xlevels)
    if (!is.null(cl <- attr(Terms, "dataClasses")))
      .checkMFClasses(cl, m)
    X <- model.matrix(Terms, m, contrasts.arg = object$contrasts)
    offset <- rep(0, nrow(X))
    if (!is.null(off.num <- attr(tt, "offset")))
      for (i in off.num) offset <- offset + eval(attr(tt,
                                                      "variables")[[i + 1]], newdata)
    if (!is.null(object$call$offset))
      offset <- offset + eval(object$call$offset, newdata)
  }
  p <- object$rank
  ord <- colnames(X)
  if (p < ncol(X) && !(missing(newdata) || is.null(newdata)))
    warning("Prediction from a rank-deficient fit may be misleading.")
  beta <- object$coefficients
  beta[is.na(beta)] <- 0
  predictor <- drop(X[, ord, drop = FALSE] %*% beta[ord])
  if (!is.null(offset))
    predictor <- predictor + offset
  if (missing(newdata) && !is.null(na.act <- object$na.action))
    predictor <- napredict(na.act, predictor)
  predictor
}
