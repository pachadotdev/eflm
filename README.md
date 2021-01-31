# boostedglm

<!-- badges: start -->
[![R-CMD-check](https://github.com/pachamaltese/yotover-testing/workflows/R-CMD-check/badge.svg)](https://github.com/pachamaltese/yotover-testing/actions)
<!-- badges: end -->

**Boostedglm** might fit *Generalized Linear Models* models up to 20 times faster than `glm()`. This packages uses just
base R and dynamic exports to make the `blm()` and `bglm()` outputs work with **broom** and **sandwich**. This takes
ideas from speedglm and fastglm.

## Installation

You can install the released version of boostedglm from GitHub with:

``` r
remotes::install_github("pachamaltese/boostedglm")
```

## Example

`boostedglm::bglm()` is ~15 times faster than `glm()` with an input data consisting in 25,830 rows and 7 columns (2 continuos, 3 binary and 2 of factor type,). See `dev/test-bglm-with-yotover-data.R`.

``` r
Unit: seconds
expr
glm(trade ~ log_dist + cntg + lang + clny + exp_year + imp_year,      family = quasipoisson(link = "log"), data = ch1_application1_2,      y = FALSE, model = FALSE)
bglm(trade ~ log_dist + cntg + lang + clny + exp_year + imp_year,      family = quasipoisson(link = "log"), data = ch1_application1_2,      y = FALSE, model = FALSE)
      min        lq      mean    median       uq        max neval
78.143386 82.797031 96.416335 85.072021 102.4835 147.671117    10
 5.842121  5.967758  6.125419  6.039139   6.1037   6.857986    10
```

## Progress list

### Sandwich

- [x] estfun
- [x] bread
- [x] vcovCL
- [x] meatCL
- [x] vcovCL
- [ ] vcovBS
- [ ] meatBS
- [ ] vcovHC
- [ ] meatHC
- [ ] vcovPC
- [ ] meatPC
- [ ] vcovPL
- [ ] meatPL

### Broom

- [x] augment
- [x] tidy
