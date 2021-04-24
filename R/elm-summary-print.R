#' @export
#' @noRd
print.summary.elm <- function(x, digits = max(3L, getOption("digits") - 3L),
                              symbolic.cor = x$symbolic.cor,
                              signif.stars = getOption("show.signif.stars"), ...) {
  cat("\nCall:\n", # S has ' ' instead of '\n'
    paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n",
    sep = ""
  )
  resid <- x$residuals
  df <- x$df
  rdf <- df[2L]
  cat(if (!is.null(x$weights) && diff(range(x$weights))) "Weighted ",
    "Residuals:\n",
    sep = ""
  )
  if (rdf > 5L) {
    nam <- c("Min", "1Q", "Median", "3Q", "Max")
    rq <- if (length(dim(resid)) == 2L) {
      structure(apply(t(resid), 1L, quantile),
        dimnames = list(nam, dimnames(resid)[[2L]])
      )
    } else {
      zz <- zapsmall(quantile(resid), digits + 1L)
      structure(zz, names = nam)
    }
    print(rq, digits = digits, ...)
  }
  else if (rdf > 0L) {
    print(resid, digits = digits, ...)
  } else { # rdf == 0 : perfect fit!
    cat("ALL", df[1L], "residuals are 0: no residual degrees of freedom!")
    cat("\n")
  }
  if (length(x$aliased) == 0L) {
    cat("\nNo Coefficients\n")
  } else {
    if (nsingular <- df[3L] - df[1L]) {
      cat("\nCoefficients: (", nsingular,
        " not defined because of singularities)\n",
        sep = ""
      )
    } else {
      cat("\nCoefficients:\n")
    }
    coefs <- x$coefficients
    if (any(aliased <- x$aliased)) {
      cn <- names(aliased)
      coefs <- matrix(NA, length(aliased), 4, dimnames = list(cn, colnames(coefs)))
      coefs[!aliased, ] <- x$coefficients
    }

    printCoefmat(coefs,
      digits = digits, signif.stars = signif.stars,
      na.print = "NA", ...
    )
  }
  ##
  cat(
    "\nResidual standard error:",
    format(signif(x$sigma, digits)), "on", rdf, "degrees of freedom"
  )
  cat("\n")
  if (nzchar(mess <- naprint(x$na.action))) cat("  (", mess, ")\n", sep = "")
  if (!is.null(x$fstatistic)) {
    cat("Multiple R-squared: ", formatC(x$r.squared, digits = digits))
    cat(
      ",\tAdjusted R-squared: ", formatC(x$adj.r.squared, digits = digits),
      "\nF-statistic:", formatC(x$fstatistic[1L], digits = digits),
      "on", x$fstatistic[2L], "and",
      x$fstatistic[3L], "DF,  p-value:",
      format.pval(pf(x$fstatistic[1L], x$fstatistic[2L],
        x$fstatistic[3L],
        lower.tail = FALSE
      ),
      digits = digits
      )
    )
    cat("\n")
  }
  correl <- x$correlation
  if (!is.null(correl)) {
    p <- NCOL(correl)
    if (p > 1L) {
      cat("\nCorrelation of Coefficients:\n")
      if (is.logical(symbolic.cor) && symbolic.cor) { # NULL < 1.7.0 objects
        print(symnum(correl, abbr.colnames = NULL))
      } else {
        correl <- format(round(correl, 2), nsmall = 2, digits = digits)
        correl[!lower.tri(correl)] <- ""
        print(correl[-1, -p, drop = FALSE], quote = FALSE)
      }
    }
  }
  cat("\n") #- not in S
  invisible(x)
}
