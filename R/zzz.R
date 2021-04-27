.onLoad <- function(...) {
  s3_register("sandwich::bread", "eglm")
  s3_register("sandwich::bread", "elm")

  invisible()
}
