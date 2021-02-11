# Dynamically exported, see zzz.R

# taken from broom::: to make augment.eglm work
as_augment_tibble <- function (data) {
  if (inherits(data, "matrix") & is.null(colnames(data))) {
    stop("The supplied `data`/`newdata` argument was an unnamed matrix. ",
         "Please supply a matrix or dataframe with column names.")
  }
  tryCatch(df <- tibble::as_tibble(data), error = function(cnd) {
    stop("Could not coerce data to `tibble`. Try explicitly passing a",
         "dataset to either the `data` or `newdata` argument.",
         call. = FALSE)
  })
  if (has_rownames(data)) {
    df <- tibble::add_column(df, .rownames = rownames(data),
                             .before = TRUE)
  }
  df
}
