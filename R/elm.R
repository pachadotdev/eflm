#' @rdname model_fitting
#' @importFrom stats gaussian na.pass
#' @export
elm <- function(formula,
                data,
                subset = NULL,
                weights = NULL,
                na.action = na.omit,
                model = TRUE,
                x = FALSE,
                y = TRUE,
                offset = NULL,
                intercept = TRUE,
                singularity.method = c("eigen", "Cholesky", "qr"),
                tol.solve = .Machine$double.eps,
                tol.values = 1e-7,
                tol.vectors = 1e-7,
                ...) {
  target <- y
  call <- match.call()
  M <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "weights", "na.action", "offset"),
             names(M), 0L)
  M <- M[c(1L, m)]
  M$drop.unused.levels <- TRUE
  M[[1L]] <- quote(stats::model.frame)
  M <- eval(M, parent.frame())
  y <- M[[1]]
  tf <- attr(M, "terms")
  X <- model.matrix(tf, M)
  offset <- model.offset(M)
  if (is.null(offset)) {
    offset <- rep(0, length(y))
  }
  if (is.null(weights)) weights <- rep(1, length(y))
  rval <- elm.wfit(
    y,
    X,
    offset = offset,
    w = weights,
    tol.solve = tol.solve,
    tol.values = tol.values,
    tol.vectors = tol.vectors,
    singularity.method = singularity.method,
    intercept = attr(tf, "intercept")
  )

  rval$terms <- tf
  rval$call <- call
  if (ncol(M) > 1) {
    for (i in 2:ncol(M)) {
      if (is.factor(M[, i])) {
        eval(parse(text = paste("rval$levels$'", names(M)[i],
          "'", "<-levels(M[,i])",
          sep = ""
        )))
      }
    }
  }
  if (model) rval$model <- M
  if (x) rval$x <- X
  if (target) rval$y <- y
  rval$fitted.values <- predict.elm(rval, newdata = data)
  rval$residuals <- y - rval$fitted.values
  rval$formula <- eval(call[[2]])
  rval
}
