#' @importFrom stats quantile setNames
#' @export
#' @keywords internal
print.summary.blm <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  x$coefficients$coef <- if (any(abs(na.omit(x$coefficients$coef)) < 0.0001)) {
    format(x$coefficients$coef,
           scientific = TRUE,
           digits = 4
    )
  } else {
    round(x$coefficients$coef, digits = 6)
  }
  x$coefficients$`Std. Error` <- if (any(na.omit(x$coefficients$`Std. Error`) < 0.0001)) {
    format(x$coefficients$`Std. Error`, scientific = TRUE, digits = 4)
  } else {
    round(x$coefficients$`Std. Error`, digits = 6)
  }
  x$coefficients$`t value` <- round(x$coefficients$`t value`, digits = 4)
  x$coefficients$`Pr(>|t|)` <- if (any(na.omit(x$coefficients$`Pr(>|t|)`) < 0.0001)) {
    format(x$coefficients$`Pr(>|t|)`,
           scientific = TRUE,
           digits = 3
    )
  } else {
    round(x$coefficients$`Pr(>|t|)`,
          digits = 6
    )
  }

  s <- sum(Vectorize(is.na(x$coefficients$`Estimate`)))
  # colnames(x$coefficients) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

  if (!is.null(x$call)) {
    cat("\nCall:\n")
    cat(deparse(x$call))
    cat("\n\n")
  }

  cat("Residuals:\n")
  x$residuals <- setNames(
    quantile(x$residuals, na.rm = TRUE),
    c("Min", "1Q", "Median", "3Q", "Max")
  )
  xx <- zapsmall(x$residuals, digits + 1L)
  print.default(xx, digits = digits, na.print = "", print.gap = 2L)
  cat("\n")

  if (length(x$coef)) {
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
  } else {
    cat("No coefficients\n")
  }
  if (x$intercept) {
    cat("Residual standard error: ", round(sqrt(x$var.res), 3), " on ", x$rdf,
        " degrees of freedom\n",
        "Multiple R-squared: ", format(x$r.squared, digits = 4),
        ",\tAdjusted R-squared: ", format(x$adj.r.squared, digits = 4), "\n",
        "F-statistic: ", format(x$fstatistic[1], digits = 4), " on ", x$fstatistic[2],
        " and ", x$fstatistic[3], " DF,\tp-value: ", format(x$f.pvalue, digits = 4),
        ".\n",
        sep = ""
    )
  } else {
    cat("Residual standard error: ", round(sqrt(x$var.res), 6), " on ", x$rdf,
        " degrees of freedom\n",
        sep = ""
    )
  }
  if (s == 1) cat("One coefficient not defined because of singularities.\n")
  if (s > 1) cat(s, " coefficients not defined because of singularities.\n")
  invisible(x)
}
