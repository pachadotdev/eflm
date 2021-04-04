#' Efficient Fitting of (Generalized) Linear Models
#'
#' Efficient Linear Model (\code{"elm"}) and Efficient Generalized Linear Model
#' (\code{"eglm"}) are used to fit linear models in an equivalent way to
#' \code{"\link{lm}"} and \code{"\link{glm}"} but in a reduced time depending on
#' the design matrix input (see the DESCRIPTION).
#'
#' @param formula an object of class \code{"\link{formula}"} (or one that can be
#' coerced to that class): a symbolic description of the model to be fitted.
#' @param data an optional data frame, list or environment (or object coercible
#' by \code{\link{as.data.frame}} to a data frame) containing the variables in
#' the model. If not found in \code{data}, the variables are taken from
#' \code{environment(formula)}, typically the environment from which \code{lm}
#' is called.
#' @param subset An optional vector specifying a subset of observations to be
#' used in the fitting process.
#' @param intercept Logical value to determine wheareas to included an intercept
#' in the null model. Defaults to \code{TRUE}.
#' @param weights an optional vector of weights to be used in the fitting
#' process. Should be \code{NULL} or a numeric vector. If non-NULL, weighted
#' least squares is used with weights \code{weights} (that is, minimizing
#' \code{sum(w*e^2)}); otherwise ordinary least squares is used.
#' @param na.action a function which indicates what should happen when the data
#' contain \code{NA}s. The default is set by the \code{na.action} setting of
#' \code{\link{options}}, and is \code{\link{na.fail}} if that is unset. The
#' \sQuote{factory-fresh} default is \code{\link{na.omit}}. Another possible
#' value is \code{NULL}, no action. Value \code{\link{na.exclude}} can be
#' useful.
#' @param offset this can be used to specify an \emph{a priori} known component
#' to be included in the linear predictor during fitting. This should be
#' \code{NULL} or a numeric vector or matrix of extents matching those of the
#' response. One or more \code{\link{offset}} terms can be included in the
#' formula instead or as well, and if more than one are specified their sum is
#' used. See \code{\link{model.offset}}.
#' @param model a logical value indicating whether model frame should be
#' included as a component of the returned value. Defaults to \code{TRUE}.
#' @param singularity.method the chosen method to detect for singularity.
#' Defaults to \code{"eigen"} but it can also be \code{"Cholesky"} or
#' \code{"qr"}.
#' @param x logical value indicating whether the model matrix used in the
#' fitting process should be returned as components of the returned value.
#' Defaults to \code{FALSE}, see the function \link{glm.fit}.
#' @param y logical value indicating whether the response vector used in the
#' fitting process should be returned as components of the returned value.
#' Defaults to \code{FALSE}, see the function \link{glm.fit})
#' @param tol.solve defaults to \code{.Machine$double.eps}, see the function
#' \link{solve}.
#' @param tol.values tolerance to consider eigenvalues equal to zero. Defaults
#' to 1e-7, see the function \link{control}.
#' @param tol.vectors tolerance to consider eigenvectors equal to zero. Defaults
#' to 1e-7, see the function \link{control}.
#' @param \dots for elm: arguments to be used to form the default control
#' argument if it is not supplied directly. For weights: further arguments
#' passed to or from other methods.
#' @param family See the function \link{glm}, but here it must be specified with
#' brackets (e.g. \code{quasipoisson()})
#' @param start Starting values for the parameters in the linear predictor
#' @param etastart Starting values for the linear predictor
#' @param mustart Starting values for the vector of means
#' @param maxit See the function \link{glm}
#' @param k The penalty per parameter to be used, the default \code{k = 2}
#' is the classical AIC
#' @param tol.estimation Tolerance to be used for the estimation
#' (Defaults to 1e-8)
#' @return an object of class "elm" that behaves the same way as the "lm" class,
#' see the function \link{lm}.
#' @examples
#' # Linear model
#' elm(mpg ~ wt, data = mtcars)
#'
#' # Generalized linear model with Gaussian link
#' eglm(mpg ~ wt, family = gaussian(), data = mtcars)
#' @name model_fitting
NULL
