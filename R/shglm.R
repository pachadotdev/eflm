# shglm <- function(formula, family = gaussian(),
#                   start = NULL, etastart = NULL, mustart = NULL, offset = NULL,
#                   maxit = 25, k = 2, ...) {
#   if (!is.null(start)) stop("Sorry, code for argument start is not implemented yet")
#   if (!is.null(mustart)) stop("Sorry, code for argument mustart is not implemented yet")
#   if (!is.null(etastart)) stop("Sorry, code for argument etastart is not implemented yet")
#   call <- match.call()
#   tf <- terms(formula, data = dati)
#   M <- model.frame(tf, dati)
#   y <- M[[1]]
#   X <- model.matrix(tf, M)
#   offset <- model.offset(M)
#   obj <- list()
#   obj$terms <- tf
#   fa <- which(attributes(attributes(M)$terms)$dataClasses == "factor")
#   if (length(fa) > 0) {
#     for (i in 1:length(fa)) {
#       eval(parse(text = paste("obj$levels$'", names(M)[fa[i]],
#                               "'", "<-levels(M[,fa[i]])",
#                               sep = ""
#       )))
#     }
#   }
#   nomicol <- colnames(dati)
#   variable <- colnames(X)
#   nobs <- length(y)
#   weights <- rep(1, nobs)
#   intercept <- attributes(tf)$intercept
#   nvar <- ncol(X)
#   if (is.null(offset)) {
#     offset <- rep.int(0, nobs)
#   }
#   sp <- NULL
#   ok <- 1:ncol(X)
#   rank <- ncol(X)
#   dfr <- nobs - rank - sum(weights == 0)
#   fam <- family$family
#   link <- family$link
#   linkfun <- family$linkfun
#   variance <- family$variance
#   dev.resids <- family$dev.resids
#   linkinv <- family$linkinv
#   mu.eta <- family$mu.eta
#   iter <- 0
#   tol <- 1
#   dev <- nobs
#   weights.cum <- wt <- wy <- nulldev <- tot.obs <- zero.weights <- 0
#   block.cum <- nrow(X)
#   end.iter <- FALSE
#
#   while ((tol > tol.estimation) & (iter < maxit)) {
#     dev.prec <- dev
#     aic.model <- dev <- Wy <- RSS <- 0
#     iter <- iter + 1
#     XTX <- matrix(0, ncol(X), ncol(X))
#     XTz <- matrix(0, ncol(X), 1)
#     eof <- FALSE
#     iter2 <- 0
#     while (!eof) {
#       iter2 <- iter2 + 1
#       if (iter > 1) {
#         if (nvar == 1) {
#           eta <- (offset + as.vector(X * start))[1:length(y)]
#         } else {
#           eta <- (offset + drop(tcrossprod(X[, ok], t(start))))[1:length(y)]
#         }
#         mu <- linkinv(eta <- eta)
#       } else {
#         if (is.null(mustart)) eval(family$initialize)
#         if (is.null(start)) {
#           eta <- (if (is.null(etastart)) linkfun(mustart) else etastart)[1:length(y)]
#           mu <- mustart[1:length(y)]
#         } else {
#           if (nvar == 1) {
#             eta <- (offset + as.vector(X * start))[1:length(y)]
#           } else {
#             eta <- (offset + as.vector(tcrossprod(X, t(start))))[1:length(y)]
#           }
#           mu <- linkinv(eta)
#         }
#       }
#       dev <- dev + sum(dev.resids(y, mu, weights))
#       if (iter > 1) aic.model <- aic.model + aic.shglm(fam, y, wt, mu, weights, dev.prec)
#       varmu <- variance(mu)
#       mu.eta.val <- mu.eta(eta)
#       z <- (eta - offset) + (y - mu) / mu.eta.val
#       W <- (weights * mu.eta.val * mu.eta.val) / varmu
#       if (iter2 == 1) {
#         XTX <- XTX + cp(X, W)
#         XTz <- XTz + t(crossprod((W * z), X))
#       } else {
#         Ax <- XTX
#         XTX <- cp(X, W)
#         XTX[rownames(Ax), colnames(Ax)] <- XTX[rownames(Ax), colnames(Ax)] + Ax
#         Az <- XTz
#         XTz <- t(crossprod((W * z), X))
#         XTz[rownames(Az), ] <- XTz[rownames(Az), ] + Az
#       }
#       res <- (y - mu) / mu.eta(eta)
#       RSS <- RSS + sum(W * res * res)
#       if (iter == 1) weights.cum <- weights.cum + sum(weights == 0)
#       if (iter == 1) {
#         ris <- control(XTX, , tol.values, tol.vectors, out.B = FALSE, method)
#         ok <- ris$pivot[1:ris$rank]
#       }
#       if (is.null(start)) start <- rep(0, rank)
#       beta <- if (iter == 1) start[ok] else start
#       start <- solve(XTX[ok, ok], XTz[ok], tol = tol.solve)
#       tol <- max(abs(dev.prec - dev) / (abs(dev) + 0.1))
#       if ((tol > tol.estimation) & (iter < maxit)) {
#         eof <- TRUE
#       } else {
#         break
#       }
#       colnames(dati) <- nomicol
#       M <- model.frame(tf, dati)
#       y <- M[[1]]
#       if (length(fa) > 0) {
#         flevels <- list()
#         j <- 0
#         for (i in 1:length(fa)) {
#           j <- j + 1
#           eval(parse(text = paste("flevels$'", names(M)[fa[i]],
#                                   "'", "<-levels(M[,fa[i]])",
#                                   sep = ""
#           )))
#           a <- c(obj$levels[[j]][!(obj$levels[[j]] %in% flevels[[j]])], flevels[[j]])
#           flevels[[j]] <- sort(a)
#         }
#         M <- model.frame(obj$terms, dati, xlev = flevels)
#         X <- model.matrix(obj$terms, M, xlev = flevels)
#         obj$levels <- flevels
#       } else {
#         X <- model.matrix(obj$terms, M)
#         flevels <- obj$levels
#       }
#       offset <- model.offset(M)
#       nobs <- length(y)
#       if (is.null(offset)) {
#         offset <- rep.int(0, nobs)
#       }
#       weights <- rep(1, nobs)
#       if (iter == 1) {
#         tot.obs <- tot.obs + nobs
#         wt <- wt + sum(weights)
#         wy <- wy + crossprod(weights, y)
#         zero.weights <- zero.weights + sum(weights == 0)
#         block.cum <- block.cum + nrow(X)
#       }
#       if (iter == 2) {
#         wtdmu <- if (intercept) wy / wt else linkinv(offset)
#         nulldev <- nulldev + sum(dev.resids(y, wtdmu, weights))
#       }
#     }
#   }
#   rank <- ris$rank
#   n.ok <- tot.obs - zero.weights
#   nulldf <- n.ok - as.integer(intercept)
#   aic.rest <- ifelse((fam %in% c(
#     "Gamma", "inverse.gaussian",
#     "gaussian"
#   )), 2, 0)
#   aic.model <- aic.model + k * rank + aic.rest
#   ll.new <- ll.fglm(fam, aic.model, rank)
#   resdf <- n.ok - rank
#   var_res <- RSS / resdf
#   dispersion <- if (fam %in% c("poisson", "binomial")) 1 else var_res
#   coefficients <- rep(NA, ncol(X))
#   start <- as(start, "numeric")
#   coefficients[ok] <- start
#   names(coefficients) <- colnames(X)
#   rval <- list(
#     coefficients = coefficients, logLik = ll.new,
#     iter = iter, tol = tol, family = family, link = link, df = resdf,
#     XTX = XTX[ok, ok], dispersion = dispersion, nok = ris$nok,
#     ok = ok, RSS = RSS, ncoll = ris$ncoll,
#     nulldev = nulldev, rank = rank, deviance = dev,
#     nulldf = nulldf, ngoodobs = n.ok, n = tot.obs, intercept = intercept,
#     aic = aic.model, convergence = (!(tol > tol.estimation)), method = "eigen"
#   )
#   rval$tf <- tf
#   rval$call <- call
#   if ((rval$iter == maxit) & (!rval$convergence)) {
#     warning("Maximum number of iterations reached without convergence")
#   }
#   class(rval) <- "fglm"
#   rval
# }
