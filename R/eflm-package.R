#' Efficient Fitting of (Generalized) Linear Models
#'
#' Efficient Linear Model (\code{"elm"}) and Efficient Generalized Linear Model
#' (\code{"eglm"}) are used to fit linear models in an equivalent way to
#' \code{"\link{lm}"} and \code{"\link{glm}"} but in a reduced time depending on
#' the design matrix input (see the DESCRIPTION).
#'
#' @param formula an object of class \code{"\link{formula}"} (or one that can be
#'  coerced to that class): a symbolic description of the model to be fitted.
#'  The details of model specification are given under \sQuote{Details}.
#' @param family a description of the error distribution and link function to be
#'  used in the model. For \code{glm} this can be a character string naming a
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
#' @param model logical value indicating whether \emph{model frame} should be
#'  included as a component of the returned value. Defaults to \code{TRUE}.
#' @param x,y logical values indicating whether the \emph{model matrix}
#'  (\code{x}) and the \emph{response vector} (\code{y}) used in the fitting
#'  process should be returned as components of the returned value. Defaults to
#'  \code{FALSE}.
#' @param intercept logical value indicating whether \emph{intercept} should be
#'  included in the \emph{null} model. Defaults to \code{TRUE}.
#' @param singularity.method the chosen method to detect for singularity.
#'  Defaults to \code{"eigen"} but it can also be \code{"Cholesky"} or
#'  \code{"qr"}. The spectral decomposition (eigenvectors decomposition) method
#'  is slower than Cholesky, but the former accepts tolerance arguments and
#'  tends to be more stable for opportune tolerance values.
#' @param tol.solve defaults to \code{.Machine$double.eps}, see the function
#'  \link{solve}.
#' @param tol.values tolerance to consider \emph{eigenvalues} equal to zero.
#'  Defaults to 1e-7.
#' @param tol.vectors tolerance to consider \emph{eigenvectors} equal to zero.
#'  Defaults to 1e-7.
#' @param tol.estimation Tolerance to be used for the estimation.
#'  Defaults to 1e-8.
#' @param maxit integer giving the maximal number of IWLS iterations.
#' @param k the penalty per parameter to be used, the default \code{k = 2}
#'  is the classical AIC.
#' @param \dots for \emph{elm} or \emph{eglm}: additional arguments to be used
#'  to form the default control. On the one hand, use \code{symmetric = F}
#'  instead of the default \code{TRUE} to indicate that the XTX matrix passed
#'  internally for least squares estimation is not symmetric. On the other, use
#'  \code{out.B = FALSE} not to return internal control's matrix output.
#'  argument if it is not supplied directly. For \emph{weights}: further
#'  arguments passed to or from other methods.
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
#' @return an object of class "elm" that behaves the same way as the "lm" class,
#'  see the function \link{lm}.
#' @examples
#' # Linear model
#' elm(mpg ~ wt, data = mtcars)
#'
#' # Generalized linear model with Gaussian link
#' eglm(mpg ~ wt, family = gaussian(), data = mtcars)
#' @name model_fitting
NULL
