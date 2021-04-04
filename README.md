# eflm

<!-- badges: start -->
[![R-CMD-check](https://github.com/pachamaltese/yotover-testing/workflows/R-CMD-check/badge.svg)](https://github.com/pachamaltese/yotover-testing/actions)
<!-- badges: end -->

Efficient Fitting of Linear and Generalized Linear Models by using
just base R. As an alternative to `lm()` and `glm()`, this package provides
`elm()` and `eglm()`, with a significant speedup when the number of 
observations is larger than the number of parameters to estimate. The speed
gains are obtained by reducing the NxP model matrix to a PxP matrix, and the 
best computational performance is obtained when R is linked against OpenBLAS,
Intel MKL or other optimized BLAS library. This implementation aims at being
compatible with 'broom' and 'sandwich' packages for summary statistics and
clustering by providing S3 methods.

This package takes ideas from glm2, speedglm, fastglm, and fixest, but the
implementations here shall keep the functions and outputs as closely as possible
to the stats package, even if that means to atenuate the speed gains.

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
