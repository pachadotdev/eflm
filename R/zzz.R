.onLoad <- function(...) {
  vctrs::s3_register("sandwich::estfun", "eglm")
  vctrs::s3_register("sandwich::estfun", "elm")
  vctrs::s3_register("sandwich::bread", "eglm")
  vctrs::s3_register("sandwich::bread", "elm")
  vctrs::s3_register("sandwich::meat", "eglm")
  vctrs::s3_register("sandwich::meat", "elm")
  vctrs::s3_register("sandwich::meatCL", "eglm")
  vctrs::s3_register("sandwich::meatCL", "elm")

  vctrs::s3_register("broom::tidy", "eglm")
  vctrs::s3_register("broom::tidy", "elm")
  vctrs::s3_register("broom::augment", "eglm")
  vctrs::s3_register("broom::augment", "elm")

  invisible()
}
