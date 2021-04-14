safe_subset <- function(d, subset) {
  r <- eval(subset, d, parent.frame())
  d[r, ]
}
