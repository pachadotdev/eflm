library(tradepolicy)
library(eflm)
library(bench)

# data ----

ch1_application2 <- agtpa_applications %>%
  select(exporter, importer, pair_id, year, trade, dist, cntg, lang, clny) %>%
  # this filter covers both OLS and PPML
  filter(year %in% seq(1986, 2006, 4)) %>%
  mutate(
    # variables for both OLS and PPML
    exp_year = paste0(exporter, year),
    imp_year = paste0(importer, year),
    year = paste0("log_dist_", year),
    log_trade = log(trade),
    log_dist = log(dist),
    smctry = ifelse(importer != exporter, 0, 1),
    
    # PPML specific variables
    log_dist_intra = log_dist * smctry,
    intra_pair = ifelse(exporter == importer, exporter, "inter")
  ) %>%
  spread(year, log_dist, fill = 0) %>%
  mutate(across(log_dist_1986:log_dist_2006, ~ .x * (1 - smctry)))

# ols ----

form <- log_trade ~ 0 + log_dist_1986 + log_dist_1990 + log_dist_1994 +
  log_dist_1998 + log_dist_2002 + log_dist_2006 + cntg +
  lang + clny + exp_year + imp_year

d <- filter(ch1_application2, importer != exporter, trade > 0)

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

form <- trade ~ 0 + log_dist_1986 + log_dist_1990 +
  log_dist_1994 + log_dist_1998 + log_dist_2002 + log_dist_2006 +
  cntg + lang + clny + exp_year + imp_year

d <- filter(ch1_application2, importer != exporter)

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

# internal distance ----

form <- trade ~ 0 + log_dist_1986 + log_dist_1990 +
  log_dist_1994 + log_dist_1998 + log_dist_2002 + log_dist_2006 +
  cntg + lang + clny + exp_year + imp_year + log_dist_intra

d <- ch1_application2

bench_internal_distance <- mark(
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

# internal distance and home bias ----

form <- trade ~ 0 + log_dist_1986 + log_dist_1990 +
  log_dist_1994 + log_dist_1998 + log_dist_2002 + log_dist_2006 +
  cntg + lang + clny + exp_year + imp_year + log_dist_intra + smctry

d <- ch1_application2

bench_home_bias <- mark(
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

# fe ----

form <- trade ~ 0 + log_dist_1986 + log_dist_1990 +
  log_dist_1994 + log_dist_1998 + log_dist_2002 + log_dist_2006 +
  cntg + lang + clny + exp_year + imp_year + intra_pair

d <- ch1_application2

bench_fe <- mark(
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

rm(d, form, ch1_application2)

save.image("benchmarks/02-chapter1-distance-puzzle-bench.RData", compress = "xz")

rm(list = ls())
gc()
