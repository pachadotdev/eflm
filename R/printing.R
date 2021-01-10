#' @importFrom stats quantile setNames
#' @export
#' @keywords internal
print.summary.fglm <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  if (!is.null(x$call)) {
    cat("\nCall:\n")
    cat(deparse(x$call))
    cat("\n\n")
  }
  cat("Deviance residuals:\n")
  if (x$df.residual > 5) {
    x$deviance.resid <- setNames(quantile(x$deviance.resid,
                                          na.rm = TRUE), c("Min", "1Q", "Median", "3Q", "Max"))
  }
  xx <- zapsmall(x$deviance.resid, digits + 1L)
  print.default(xx, digits = digits, na.print = "", print.gap = 2L)
  cat("\n")
  if (length(x$coef) > 0) {
    cat("Coefficients:\n")
    sig <- function(z) {
      if (!is.na(z)) {
        if (z < 0.001) {
          "***"
        } else if (z < 0.01) {
          "** "
        } else if (z < 0.05) {
          "*  "
        } else if (z < 0.1) {
          ".  "
        } else {
          "   "
        }
      } else {
        "   "
      }
    }
    options(warn = -1)
    sig.1 <- sapply(
      as.numeric(as.character(x$coefficients[, 4])),
      sig
    )
    options(warn = 0)
    est.1 <- cbind(
      format(x$coefficients, digits = digits),
      sig.1
    )
    colnames(est.1)[ncol(est.1)] <- ""
    print(est.1)
    cat("---")
    cat("\n")
    cat(
      "Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
      "\n"
    )
    cat("\n")
  }
  else {
    cat("No coefficients\n")
  }
  cat("(Dispersion parameter for ", x$family$family, " family taken to be ",
      format(x$dispersion), ")\n\n", apply(cbind(paste(format(
        c("Null", "Residual"), justify = "right"), "deviance:"),
        format(unlist(x[c("null.deviance", "deviance")]), digits = max(5L, digits + 1L)), " on",
        format(unlist(x[c("df.null", "df.residual")])),
        " degrees of freedom\n"), 1L, paste, collapse = " "
      ), sep = "")

  cat("AIC:", format(x$aic, digits = max(4L, digits + 1L)))
  cat("\n\n")
  cat("Number of Fisher Scoring iterations:", x$iter)
  invisible(x)
}

#' @importFrom stats coef naprint
#' @export
#' @keywords internal
print.fglm <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("\nCall:  ", paste(deparse(x$call), sep = "\n", collapse = "\n"),
      "\n\n", sep = "")
  if (length(coef(x))) {
    cat("Coefficients")
    if (is.character(co <- x$contrasts))
      cat("  [contrasts: ", apply(cbind(names(co), co),
                                  1L, paste, collapse = "="), "]")
    cat(":\n")
    print.default(format(x$coefficients, digits = digits),
                  print.gap = 2, quote = FALSE)
  }
  else cat("No coefficients\n\n")
  cat("\nDegrees of Freedom:", x$df.null, "Total (i.e. Null); ",
      x$df.residual, "Residual\n")
  if (nzchar(mess <- naprint(x$na.action)))
    cat("  (", mess, ")\n", sep = "")
  cat("Null Deviance:\t   ", format(signif(x$null.deviance,
                                           digits)), "\nResidual Deviance:", format(signif(x$deviance,digits)),
      "\tAIC:", format(signif(x$aic, digits)))
  cat("\n")
  invisible(x)
}

#' @export
#' @importFrom stats logLik
#' @keywords internal
print.logLik.fglm <- function(x, digits = getOption("digits"), ...) {
  cat("'log Lik.' ", paste(format(logLik(x), digits = digits), collapse = ", "),
      " (df=", format(attr(x, "df")), ")\n",
      sep = ""
  )
  invisible(x)
}
