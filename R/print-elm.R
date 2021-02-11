#' @importFrom stats coef naprint
#' @export
#' @keywords internal
print.elm <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  if (!is.null(x$call)) {
    cat("\nCall:\n")
    cat(deparse(x$call))
    cat("\n\n")
  }
  if (length(x$coef)) {
    cat("Coefficients:\n")
    print.default(format(x$coefficients, digits = digits),
                  print.gap = 2,
                  quote = FALSE
    )
  } else {
    cat("No coefficients\n")
  }
  cat("\n")
  invisible(x)
}
