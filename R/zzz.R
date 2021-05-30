.onLoad <- function(...) {
  s3_register("sandwich::bread", "eglm")
  s3_register("sandwich::bread", "elm")

  s3_register("broom::tidy", "elm")
  s3_register("broom::tidy", "eglm")

  s3_register("broom::augment", "elm")
  s3_register("broom::augment", "eglm")

  s3_register("broom::glance", "elm")
  s3_register("broom::glance", "eglm")

  return(invisible())
}
