# Dynamically exported, see zzz.R

# taken from broom::: to make augment.bglm work
has_rownames <- function (df) {
  if (tibble::is_tibble(df)) {
    return(FALSE)
  }
  any(rownames(df) != as.character(1:nrow(df)))
}
