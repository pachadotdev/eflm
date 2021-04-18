utils::globalVariables("n", add = TRUE)

#' Efficient Fitting of Generalized Linear Models
#'
#' Efficient Generalized Linear Model (\code{"eglm"}) is used to fit generalized
#'  linear models in an equivalent way to \code{"\link{glm}"} but in a reduced
#'  time depending on the design matrix and the family (or link).
#'
#' @param formula an object of class \code{"\link{formula}"} (or one that can be
#'  coerced to that class): a symbolic description of the model to be fitted.
#'  The details of model specification are given under \sQuote{Details}.
#' @param family a description of the error distribution and link function to be
#'  used in the model. This can be a character string naming a
#'  family function, a family function or the result of a call to a family
#'  function. See \code{\link{family}} for details of family functions.
#' @param data an optional data frame, list or environment (or object coercible
#'  by \code{\link{as.data.frame}} to a data frame) containing the variables in
#'  the model. If not found in \code{data}, the variables are taken from
#'  \code{environment(formula)}, typically the environment from which \code{lm}
#'  is called.
#' @param weights an optional vector of weights to be used in the fitting
#'  process. Should be \code{NULL} or a numeric vector. If non-NULL, weighted
#'  least squares is used with weights \code{weights} (that is, minimizing
#'  \code{sum(w*e^2)}); otherwise ordinary least squares is used.
#' @param subset an optional vector specifying a subset of observations to be
#'  used in the fitting process.
#' @param na.action a function which indicates what should happen when the data
#'  contain \code{NA}s. The default is set by the \code{na.action} setting of
#'  \code{\link{options}}, and is \code{\link{na.fail}} if that is unset. The
#'  \sQuote{factory-fresh} default is \code{\link{na.omit}}. Another possible
#'  value is \code{NULL}, no action. Value \code{\link{na.exclude}} can be
#'  useful.
#' @param start starting values for the parameters in the linear predictor.
#' @param etastart starting values for the linear predictor.
#' @param mustart starting values for the vector of means.
#' @param offset this can be used to specify an \emph{a priori} known component
#'  to be included in the linear predictor during fitting. This should be
#'  \code{NULL} or a numeric vector or matrix of extents matching those of the
#'  response. One or more \code{\link{offset}} terms can be included in the
#'  formula instead or as well, and if more than one are specified their sum is
#'  used. See \code{\link{model.offset}}.
#' @param control a list of parameters for controlling the fitting process. For
#'  \code{eglm.wfit} this is passed to \code{eglm.control}.
#' @param model logical value indicating whether \emph{model frame} should be
#'  included as a component of the returned value. Defaults to \code{TRUE}.
#' @param method the method to be used in fitting the model. The default method
#'  \code{"eglm.wfit"} uses iteratively reweighted least squares (IWLS): the
#'  alternative \code{"model.frame"} returns the model frame and does no
#'  fitting. User-supplied fitting functions can be supplied either as a
#'  function or a character string naming a function, with a function which
#'  takes the same arguments as \code{glm.fit} from the \bold{stats} package.
#'  If specified as a character string it is looked up from within the
#'  \bold{eflm} namespace.
#' @param x,y logical values indicating whether the
#'  \emph{model matrix} (\code{x}) and the \emph{response vector} (\code{y})
#'  used in the fitting process should be returned as components of the returned
#'  value.
#' @param singular.ok logical; if FALSE a singular fit is an error.
#' @param contrasts an optional list. See the \code{contrasts.arg} of
#'  \code{model.matrix.default}.
#' @param reduce logical. Should an alternate design matrix of \code{p * p} be
#'  used instead of the traditional \code{n * p} design matrix
#' @param \dots For eglm: arguments to be used to form the default control
#'  argument if it is not supplied directly. For weights: further arguments
#'  passed to or from other methods.
#'
#' @details Models for \code{elm} and \code{eglm} are specified symbolically.
#'  A typical model has the form \code{response ~ terms} where \code{response}
#'  is the (numeric) response vector and \code{terms} is a series of terms which
#'  specifies a linear predictor for \code{response}. A terms specification of
#'  the form \code{first + second} indicates all the terms in \code{first}
#'  together with all the terms in \code{second} with duplicates removed. A
#'  specification of the form \code{first:second} indicates the set of
#'  terms obtained by taking the interactions of all terms in \code{first}
#'  with all terms in \code{second}. The specification \code{first*second}
#'  indicates the \emph{cross} of \code{first} and \code{second}. This is
#'  the same as \code{first + second + first:second}, and exactly the same as
#'  \code{"\link{lm}"} and \code{"\link{glm}"} from the \link{stats} package.
#'
#' @return an object of class "elm" that behaves the same way as the "lm" class,
#'  see the function \link{lm}.
#'
#' @examples
#' eglm(mpg ~ wt, family = gaussian, data = mtcars)
#' @importFrom stats gaussian na.pass model.response is.empty.model
#'  model.matrix model.weights model.offset model.extract .getXlevels
#'
#' @export
eglm <- function(formula, family = gaussian, data, weights,
                 subset, na.action, start = NULL,
                 etastart, mustart, offset,
                 control = list(...),
                 model = TRUE, method = "eglm.wfit",
                 x = FALSE, y = TRUE,
                 singular.ok = TRUE, contrasts = NULL,
                 reduce = FALSE, ...) {
  cal <- match.call()
  ## family
  if (is.character(family)) {
    family <- get(family, mode = "function", envir = parent.frame())
  }
  if (is.function(family)) family <- family()
  if (is.null(family$family)) {
    print(family)
    stop("'family' not recognized")
  }

  ## extract x, y, etc from the model formula and frame
  if (missing(data)) data <- environment(formula)
  mf <- match.call(expand.dots = FALSE)
  m <- match(c(
    "formula", "data", "subset", "weights", "na.action",
    "etastart", "mustart", "offset"
  ), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  ## need stats:: for non-standard evaluation
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  if (identical(method, "model.frame")) {
    return(mf)
  }

  if (!is.character(method) && !is.function(method)) {
    stop("invalid 'method' argument")
  }
  ## for back-compatibility in return result
  ## unlike stats::, this package just has wfit
  if (identical(method, "eglm.wfit")) {
    control <- do.call("eglm.control", control)
  }

  mt <- attr(mf, "terms") # allow model.frame to have updated it

  Y <- model.response(mf, "any") # e.g. factors are allowed
  ## avoid problems with 1D arrays, but keep names
  if (length(dim(Y)) == 1L) {
    nm <- rownames(Y)
    dim(Y) <- NULL
    if (!is.null(nm)) names(Y) <- nm
  }
  ## null model support
  X <- if (!is.empty.model(mt)) model.matrix(mt, mf, contrasts) else matrix(, NROW(Y), 0L)
  ## avoid any problems with 1D or nx1 arrays by as.vector.
  weights <- as.vector(model.weights(mf))
  if (!is.null(weights) && !is.numeric(weights)) {
    stop("'weights' must be a numeric vector")
  }
  ## check weights and offset
  if (!is.null(weights) && any(weights < 0)) {
    stop("negative weights not allowed")
  }

  offset <- as.vector(model.offset(mf))
  if (!is.null(offset)) {
    if (length(offset) != NROW(Y)) {
      stop(gettextf(
        "number of offsets is %d should equal %d (number of observations)",
        length(offset), NROW(Y)
      ), domain = NA)
    }
  }
  ## these allow starting values to be expressed in terms of other vars.
  mustart <- model.extract(mf, "mustart")
  etastart <- model.extract(mf, "etastart")

  ## We want to set the name on this call and the one below for the
  ## sake of messages from the fitter function
  fit <- eval(call(if (is.function(method)) "method" else method,
    x = X, y = Y, weights = weights, start = start,
    etastart = etastart, mustart = mustart,
    offset = offset, family = family, control = control,
    intercept = attr(mt, "intercept") > 0L, singular.ok = singular.ok,
    reduce = reduce
  ))

  ## This calculated the null deviance from the intercept-only model
  ## if there is one, otherwise from the offset-only model.
  ## We need to recalculate by a proper fit if there is intercept and
  ## offset.
  ##
  ## The eglm.wfit calculation could be wrong if the link depends on the
  ## observations, so we allow the null deviance to be forced to be
  ## re-calculated by setting an offset (provided there is an intercept).
  ## Prior to 2.4.0 this was only done for non-zero offsets.
  if (length(offset) && attr(mt, "intercept") > 0L) {
    fit2 <-
      eval(call(if (is.function(method)) "method" else method,
        x = X[, "(Intercept)", drop = FALSE], y = Y,
        weights = weights, offset = offset, family = family,
        control = control, intercept = TRUE
      ))
    ## That fit might not have converged ....
    if (!fit2$converged) {
      warning("fitting to calculate the null deviance did not converge -- increase 'maxit'?")
    }
    fit$null.deviance <- fit2$deviance
  }
  if (model) fit$model <- mf
  fit$na.action <- attr(mf, "na.action")
  if (x) fit$x <- X
  if (!y) fit$y <- NULL
  structure(c(
    fit,
    list(
      call = cal, formula = formula,
      terms = mt, data = data,
      offset = offset, control = control, method = method,
      contrasts = attr(X, "contrasts"),
      xlevels = .getXlevels(mt, mf)
    )
  ),
  class = c(fit$class, c("eglm", "glm", "lm"))
  )
}
