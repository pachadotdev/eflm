#' @export
#' @importFrom stats logLik
#' @keywords internal
print.logLik.eglm <- function(x, digits = getOption("digits"), ...) {
  cat("'log Lik.' ", paste(format(logLik(x), digits = digits), collapse = ", "),
    " (df=", format(attr(x, "df")), ")\n",
    sep = ""
  )
  invisible(x)
}
