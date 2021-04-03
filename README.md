# eflm

<!-- badges: start -->
[![R-CMD-check](https://github.com/pachamaltese/yotover-testing/workflows/R-CMD-check/badge.svg)](https://github.com/pachamaltese/yotover-testing/actions)
<!-- badges: end -->

**Eflm** might fit *Generalized Linear Models* models up to 20 times faster than `glm()`. This packages uses just base R and dynamic exports to make the `elm()` and `eglm()` outputs work with **broom** and **sandwich**. This takes ideas from glm2, speedglm and fastglm.

## Installation

You can install the released version of eflm from CRAN with:
```r
install.packages("eflm")
```

And the development version with:
``` r
remotes::install_github("pachamaltese/boostedglm")
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
