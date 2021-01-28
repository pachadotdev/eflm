.onLoad <- function(...) {
  vctrs::s3_register("sandwich::estfun", "bglm")
  vctrs::s3_register("sandwich::bread", "bglm")
  vctrs::s3_register("sandwich::meatCL", "bglm")

  vctrs::s3_register("broom::tidy", "bglm")
  vctrs::s3_register("broom::augment", "bglm")

  invisible()
}
