#' Predict Method for eglm Fits
#'
#' Obtains predictions and optionally estimates standard errors of
#' those predictions from a fitted generalized linear model object (see the
#' function \link{predict.glm}).
#'
#' @param object a fitted object of class inheriting from "eglm"
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
#' @return An object of class "numeric" or "matrix", see the function \link{predict.glm}.
#' @importFrom stats family fitted .checkMFClasses
#' @export
#' @keywords internal
predict.eglm <- function(object, newdata, se.fit = FALSE, scale = NULL, df = Inf,
                         interval = c("none", "confidence", "prediction"), level = 0.95,
                         type = c("link", "response", "terms"), terms = NULL, na.action = na.pass,
                         weights = NULL, ...) {
  type <- match.arg(type)
  if (missing(newdata) & is.null(object$linear.predictors)) {
    warning("Fitted values were not returned from the eglm object:
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
    pred <- predict.elm(object, newdata,
      type = "response",
      na.action = na.action
    )
    switch(type,
      response = {
        pred <- stats::family(object)$linkinv(pred)
      },
      link =
      )
  }
  pred
}
