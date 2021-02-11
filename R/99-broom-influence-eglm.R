# Dynamically exported, see zzz.R

# taken from stats::, defined to use broom::augment
#' @importFrom stats lm.influence
influence.eglm <- function (model, do.coef = TRUE, ...) {
  res <- lm.influence(model, do.coef = do.coef, ...)
  pRes <- na.omit(residuals(model, type = "pearson"))[model$prior.weights !=
                                                        0]
  pRes <- naresid(model$na.action, pRes)
  names(res)[names(res) == "wt.res"] <- "dev.res"
  c(res, list(pear.res = pRes))
}
