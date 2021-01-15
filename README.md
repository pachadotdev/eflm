
# boostedglm

<!-- badges: start -->
<!-- badges: end -->

The goal of boostedglm is to provide functions for efficient fitting of generalized linear models. Unlike other similar packages, `boostedglm` uses base R only and aims at being aligned with glm's output and to be integrated with sandwich, stargazer and other useful packages for regression.

## Installation

You can install the released version of boostedglm from GitHub with:

``` r
remotes::install_github("pachamaltese/boostedglm")
```

## Example

`boostedglm::fglm()` is ~15 times faster than `glm()` with an input data consisting in 25,830 rows and 7 columns (2 continuos, 3 binary and 2 of factor type,). See `dev/test-fglm-with-yotover-data.R`.

``` r
Unit: seconds
expr
glm(trade ~ log_dist + cntg + lang + clny + exp_year + imp_year,      family = quasipoisson(link = "log"), data = ch1_application1_2,      y = FALSE, model = FALSE)
fglm(trade ~ log_dist + cntg + lang + clny + exp_year + imp_year,      family = quasipoisson(link = "log"), data = ch1_application1_2,      y = FALSE, model = FALSE)
min        lq      mean    median       uq        max neval
78.143386 82.797031 96.416335 85.072021 102.4835 147.671117    10
5.842121  5.967758  6.125419  6.039139   6.1037   6.857986    10
```
