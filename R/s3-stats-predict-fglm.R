#' @importFrom stats family fitted .checkMFClasses
#' @export
predict.fglm <- function(object, newdata = NULL, type = c("link", "response", "terms"),
                         se.fit = FALSE, dispersion = NULL, terms = NULL,
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
    pred <- predict.flm(object, newdata, type = "response",
      na.action = na.action
    )
    switch(type, response = {
      pred <- stats::family(object)$linkinv(pred)
    }, link = )
  }
  pred
}
