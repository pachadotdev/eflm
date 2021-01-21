#' Predict Method for FGLM Fits
#'
#' Obtains predictions and optionally estimates standard errors of
#' those predictions from a fitted generalized linear model object (see the
#' function \link{predict.glm}).
#'
#' @param object a fitted object of class inheriting from "fglm"
#' @param newdata optionally, a data frame in which to look for variables with
#' which to predict. If omitted, the fitted linear predictors are used
#' @param se.fit EXPLAIN
#' @param scale EXPLAIN
#' @param df NULL
#' @param interval EXPLAIN
#' @param level EXPLAIN
#' @param type the type of prediction required. The default is on the scale of
#' the linear predictors; the alternative "response" is on the scale of the
#' response variable. Thus for a default binomial model the default predictions
#' are of log-odds (probabilities on logit scale) and type = "response" gives
#' the predicted probabilities. The "terms" option returns a matrix giving the
#' fitted values of each term in the model formula on the linear predictor
#' scale
#' @param terms EXPLAIN
#' @param na.action function determining what should be done with missing values
#' in newdata. The default is to predict NA
#' @param pred.var EXPLAIN
#' @param weights EXPLAIN
#' @param ... further arguments passed to or from other methods
#' @importFrom stats family fitted .checkMFClasses
#' @export
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
