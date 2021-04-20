#' @importFrom stats naprint printCoefmat symnum quantile
#' @export
#' @noRd
print.summary.eglm <- function(x, digits = max(3L, getOption("digits") - 3L),
                               symbolic.cor = x$symbolic.cor,
                               signif.stars = getOption("show.signif.stars"), ...) {
  cat("\nCall:\n",
    paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n",
    sep = ""
  )
  cat("Deviance Residuals: \n")
  if (x$df.residual > 5) {
    x$deviance.resid <- setNames(
      quantile(x$deviance.resid, na.rm = TRUE),
      c("Min", "1Q", "Median", "3Q", "Max")
    )
  }
  xx <- zapsmall(x$deviance.resid, digits + 1L)
  print.default(xx, digits = digits, na.print = "", print.gap = 2L)

  if (length(x$aliased) == 0L) {
    cat("\nNo Coefficients\n")
  } else {
    df <- if ("df" %in% names(x)) x[["df"]] else NULL
    if (!is.null(df) && (nsingular <- df[3L] - df[1L])) {
      cat("\nCoefficients: (", nsingular,
        " not defined because of singularities)\n",
        sep = ""
      )
    } else {
      cat("\nCoefficients:\n")
    }
    coefs <- x$coefficients
    if (!is.null(aliased <- x$aliased) && any(aliased)) {
      cn <- names(aliased)
      coefs <- matrix(NA, length(aliased), 4L,
        dimnames = list(cn, colnames(coefs))
      )
      coefs[!aliased, ] <- x$coefficients
    }
    printCoefmat(coefs,
      digits = digits, signif.stars = signif.stars,
      na.print = "NA", ...
    )
  }

  cat("\n(Dispersion parameter for ", x$family$family,
    " family taken to be ", format(x$dispersion), ")\n\n",
    apply(cbind(
      paste(
        format(c("Null", "Residual"), justify = "right"),
        "deviance:"
      ),
      format(unlist(x[c("null.deviance", "deviance")]),
        digits = max(5L, digits + 1L)
      ), " on",
      format(unlist(x[c("df.null", "df.residual")])),
      " degrees of freedom\n"
    ),
    1L, paste,
    collapse = " "
    ),
    sep = ""
  )
  if (nzchar(mess <- naprint(x$na.action))) cat("  (", mess, ")\n", sep = "")
  cat("AIC: ", format(x$aic, digits = max(4L, digits + 1L)), "\n\n",
    "Number of Fisher Scoring iterations: ", x$iter,
    "\n",
    sep = ""
  )

  correl <- x$correlation
  if (!is.null(correl)) {
    p <- NCOL(correl)
    if (p > 1) {
      cat("\nCorrelation of Coefficients:\n")
      if (is.logical(symbolic.cor) && symbolic.cor) {
        print(symnum(correl, abbr.colnames = NULL))
      } else {
        correl <- format(round(correl, 2L), nsmall = 2L, digits = digits)
        correl[!lower.tri(correl)] <- ""
        print(correl[-1, -p, drop = FALSE], quote = FALSE)
      }
    }
  }
  cat("\n")
  invisible(x)
}
