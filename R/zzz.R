.onLoad <- function(...) {
  vctrs::s3_register("sandwich::estfun", "fglm")
  vctrs::s3_register("sandwich::bread", "fglm")
  vctrs::s3_register("sandwich::meatCL", "fglm")

  vctrs::s3_register("broom::tidy", "fglm")
  invisible()
}
