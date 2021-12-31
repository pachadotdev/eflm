library(tradepolicy)
library(eflm)
library(bench)

# data ----

ch1_application3 <- agtpa_applications %>%
  filter(year %in% seq(1986, 2006, 4)) %>%
  mutate(
    exp_year = paste0(exporter, year),
    imp_year = paste0(importer, year),
    year = paste0("intl_border_", year),
    log_trade = log(trade),
    log_dist = log(dist),
    intl_brdr = ifelse(exporter == importer, pair_id, "inter"),
    intl_brdr_2 = ifelse(exporter == importer, 0, 1),
    pair_id_2 = ifelse(exporter == importer, "0-intra", pair_id)
  ) %>%
  spread(year, intl_brdr_2, fill = 0)

ch1_application3 <- ch1_application3 %>%
  group_by(pair_id) %>%
  mutate(sum_trade = sum(trade)) %>%
  ungroup()

# ols ----

form <- log_trade ~ 0 + log_dist + cntg + lang + clny +
  rta + exp_year + imp_year

d <- filter(ch1_application3, importer != exporter, trade > 0)

bench_ols <- mark(
  lm(
    form,
    data = d,
    y = FALSE,
    model = FALSE
  )$coefficients,
  elm(
    form,
    data = d,
    y = FALSE,
    model = FALSE,
    reduce = TRUE
  )$coefficients
)

rm(d)

# ppml ----

form <- trade ~ 0 + log_dist + cntg + lang + clny +
  rta + exp_year + imp_year

d <- filter(ch1_application3, importer != exporter)

bench_ppml <- mark(
  glm(
    form,
    family = stats::quasipoisson(link = "log"),
    data = d,
    y = FALSE,
    model = FALSE
  )$coefficients,
  eglm(
    form,
    family = stats::quasipoisson(link = "log"),
    data = d,
    y = FALSE,
    model = FALSE,
    reduce = TRUE
  )$coefficients
)

rm(d)

# trade diversion ----

form <- trade ~ 0 + log_dist + cntg + lang + clny +
  rta + exp_year + imp_year + intl_brdr

d <- ch1_application3

bench_trade_diversion <- mark(
  glm(
    form,
    family = stats::quasipoisson(link = "log"),
    data = d,
    y = FALSE,
    model = FALSE
  )$coefficients,
  eglm(
    form,
    family = stats::quasipoisson(link = "log"),
    data = d,
    y = FALSE,
    model = FALSE,
    reduce = TRUE
  )$coefficients
)

rm(d)

# endogeneity ----

form <- trade ~ 0 + rta + exp_year + imp_year + pair_id_2

d <- filter(ch1_application3, sum_trade > 0)

bench_endogeneity <- mark(
  glm(
    form,
    family = stats::quasipoisson(link = "log"),
    data = d,
    y = FALSE,
    model = FALSE
  )$coefficients,
  eglm(
    form,
    family = stats::quasipoisson(link = "log"),
    data = d,
    y = FALSE,
    model = FALSE,
    reduce = TRUE
  )$coefficients
)

rm(d)

# reverse causality ----

form <- trade ~ 0 + rta + rta_lead4 + exp_year + imp_year + pair_id_2

d <- filter(ch1_application3, sum_trade > 0)

bench_reverse_causality <- mark(
  glm(
    form,
    family = stats::quasipoisson(link = "log"),
    data = d,
    y = FALSE,
    model = FALSE
  )$coefficients,
  eglm(
    form,
    family = stats::quasipoisson(link = "log"),
    data = d,
    y = FALSE,
    model = FALSE,
    reduce = TRUE
  )$coefficients
)

rm(d)

# non-linear/phasing effects ----

form <- trade ~ 0 + rta + rta_lag4 + rta_lag8 + rta_lag12 +
  exp_year + imp_year + pair_id_2

d <- filter(ch1_application3, sum_trade > 0)

bench_phasing <- mark(
  glm(
    form,
    family = stats::quasipoisson(link = "log"),
    data = d,
    y = FALSE,
    model = FALSE
  )$coefficients,
  eglm(
    form,
    family = stats::quasipoisson(link = "log"),
    data = d,
    y = FALSE,
    model = FALSE,
    reduce = TRUE
  )$coefficients
)

rm(d)

# globalization ----

form <- trade ~ 0 + rta + rta_lag4 + rta_lag8 + rta_lag12 +
  intl_border_1986 + intl_border_1990 + intl_border_1994 +
  intl_border_1998 + intl_border_2002 +
  exp_year + imp_year + pair_id_2

d <- filter(ch1_application3, sum_trade > 0)

bench_globalization <- mark(
  glm(
    form,
    family = stats::quasipoisson(link = "log"),
    data = d,
    y = FALSE,
    model = FALSE
  )$coefficients,
  eglm(
    form,
    family = stats::quasipoisson(link = "log"),
    data = d,
    y = FALSE,
    model = FALSE,
    reduce = TRUE
  )$coefficients
)

rm(d, form, ch1_application3)

save.image("benchmarks/03-chapter1-regional-trade-agreements-bench.RData", compress = "xz")

rm(list = ls())
gc()
