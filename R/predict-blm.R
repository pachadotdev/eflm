#' @export
#' @importFrom stats napredict delete.response
#' @keywords internal
predict.blm <- function(object, newdata, se.fit = FALSE, scale = NULL, df = Inf,
                        interval = c("none", "confidence", "prediction"), level = 0.95,
                        type = c("response", "terms"), terms = NULL, na.action = na.pass,
                        pred.var = res.var/weights, weights = 1, ...) {
  tt <- terms(object)
  if (!inherits(object, c("blm", "bglm"))) {
    warning("calling predict.blm(<fake-blm/bglm-object>) ...")
  }
  if (missing(newdata) || is.null(newdata)) {
    if (is.null(object$fitted.values)) {
      warning("Fitted values were not returned from the bglm object:
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
  if (se.fit || all(!interval %in% "none")) {
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
      piv <- if (p > 0) object$qr$pivot[p1]
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
  if (se.fit || all(!interval %in% "none")) {
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
