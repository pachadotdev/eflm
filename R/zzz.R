.onLoad <- function(...) {
  vctrs::s3_register("sandwich::estfun", "eglm")
  vctrs::s3_register("sandwich::bread", "eglm")
  vctrs::s3_register("sandwich::meatCL", "eglm")
  vctrs::s3_register("sandwich::vcovBS", "eglm")

  vctrs::s3_register("broom::tidy", "eglm")
  vctrs::s3_register("broom::augment", "eglm")

  invisible()
}
