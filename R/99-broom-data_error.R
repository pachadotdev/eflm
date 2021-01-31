# Dynamically exported, see zzz.R

# taken from brooom::: to make augment.bglm work
data_error <- function (cnd) {
  stop("Must specify either `data` or `newdata` argument.",
       call. = FALSE)
}
