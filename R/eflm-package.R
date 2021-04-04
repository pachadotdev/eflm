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
#'  \code{"qr"}.
#' @param tol.solve defaults to \code{.Machine$double.eps}, see the function
#'  \link{solve}.
#' @param tol.values tolerance to consider \emph{eigenvalues} equal to zero.
#'  Defaults to 1e-7.
#' @param tol.vectors tolerance to consider \emph{eigenvectors} equal to zero.
#'  Defaults to 1e-7.
#' @param tol.estimation Tolerance to be used for the estimation.
#'  Defaults to 1e-8.
#' @param \dots for elm: arguments to be used to form the default control
#'  argument if it is not supplied directly. For weights: further arguments
#'  passed to or from other methods.
#' @param maxit See the function \link{glm}
#' @param k The penalty per parameter to be used, the default \code{k = 2}
#'  is the classical AIC
#' @details Models for \code{lm} are specified symbolically. A typical model
#'  has the form \code{response ~ terms} where \code{response} is the (numeric)
#'  response vector and \code{terms} is a series of terms which specifies a
#'  linear predictor for \code{response}.  A terms specification of the form
#'  \code{first + second} indicates all the terms in \code{first} together
#'  with all the terms in \code{second} with duplicates removed.  A
#'  specification of the form \code{first:second} indicates the set of
#'  terms obtained by taking the interactions of all terms in \code{first}
#'  with all terms in \code{second}.  The specification \code{first*second}
#'  indicates the \emph{cross} of \code{first} and \code{second}.  This is
#'  the same as \code{first + second + first:second}.
#'
#'  If the formula includes an \code{\link{offset}}, this is evaluated and
#'  subtracted from the response.
#'
#'  If \code{response} is a matrix a linear model is fitted separately by
#'  least-squares to each column of the matrix.
#'
#'  See \code{\link{model.matrix}} for some further details.  The terms in
#'  the formula will be re-ordered so that main effects come first,
#'  followed by the interactions, all second-order, all third-order and so
#'  on: to avoid this pass a \code{terms} object as the formula (see
#'  \code{\link{aov}} and \code{demo(glm.vr)} for an example).
#'
#'  A formula has an implied intercept term.  To remove this use either
#'  \code{y ~ x - 1} or \code{y ~ 0 + x}. See \code{\link{formula}} for
#'  more details of allowed formulae.
#'
#'  Non-\code{NULL} \code{weights} can be used to indicate that
#'  different observations have different variances (with the values in
#'  \code{weights} being inversely proportional to the variances); or
#'  equivalently, when the elements of \code{weights} are positive
#'  integers \eqn{w_i}, that each response \eqn{y_i} is the mean of
#'  \eqn{w_i} unit-weight observations (including the case that there
#'  are \eqn{w_i} observations equal to \eqn{y_i} and the data have been
#'  summarized). However, in the latter case, notice that within-group
#'  variation is not used.  Therefore, the sigma estimate and residual
#'  degrees of freedom may be suboptimal; in the case of replication
#'  weights, even wrong. Hence, standard errors and analysis of variance
#'  tables should be treated with care.
#'
#'  \code{lm} calls the lower level functions \code{\link{lm.fit}}, etc,
#'  see below, for the actual numerical computations.  For programming
#'  only, you may consider doing likewise.
#'
#'  All of \code{weights}, \code{subset} and \code{offset} are evaluated
#'  in the same way as variables in \code{formula}, that is first in
#'  \code{data} and then in the environment of \code{formula}.
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
