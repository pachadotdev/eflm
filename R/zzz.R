.onLoad <- function(...) {
  s3_register("sandwich::bread", "eglm")
  s3_register("sandwich::bread", "elm")

  s3_register("broom::tidy", "elm")

  invisible()
}
