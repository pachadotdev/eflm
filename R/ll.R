ll.fglm <- function(family, aic.model, nvar) {
  switch(family,
    binomial = -(aic.model - 2 * nvar) / 2,
    Gamma = -((aic.model - 2 * nvar) - 2) / 2,
    gaussian = -((aic.model - 2 * nvar) - 2) / 2,
    poisson = -(aic.model - 2 * nvar) / 2,
    inverse.gaussian = -((aic.model - 2 * nvar) - 2) / 2,
    quasi = NA,
    quasibinomial = NA,
    quasipoisson = NA
  )
}
