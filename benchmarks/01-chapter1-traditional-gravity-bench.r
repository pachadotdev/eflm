library(tradepolicy)
library(eflm)
library(bench)

# data ----

ch1_application1 <-  agtpa_applications %>%
  select(exporter, importer, pair_id, year, trade, dist, cntg, lang, clny) %>%
  filter(year %in% seq(1986, 2006, 4))

ch1_application1 <- ch1_application1 %>%
  mutate(
    log_trade = log(trade),
    log_dist = log(dist)
  )

ch1_application1 <- ch1_application1 %>%
  # Create Yit
  group_by(exporter, year) %>%
  mutate(
    y = sum(trade),
    log_y = log(y)
  ) %>%
  
  # Create Eit
  group_by(importer, year) %>%
  mutate(
    e = sum(trade),
    log_e = log(e)
  )

ch1_application1 <- ch1_application1 %>%
  # Replicate total_e
  group_by(exporter, year) %>%
  mutate(total_e = sum(e)) %>%
  group_by(year) %>%
  mutate(total_e = max(total_e)) %>%
  
  # Replicate rem_exp
  group_by(exporter, year) %>%
  mutate(
    remoteness_exp = sum(dist *  total_e / e),
    log_remoteness_exp = log(remoteness_exp)
  ) %>%
  
  # Replicate total_y
  group_by(importer, year) %>%
  mutate(total_y = sum(y)) %>%
  group_by(year) %>%
  mutate(total_y = max(total_y)) %>%
  
  # Replicate rem_imp
  group_by(importer, year) %>%
  mutate(
    remoteness_imp = sum(dist / (y / total_y)),
    log_remoteness_imp = log(remoteness_imp)
  )

ch1_application1 <- ch1_application1 %>%
  # This merges the columns exporter/importer with year
  mutate(
    exp_year = paste0(exporter, year),
    imp_year = paste0(importer, year)
  )

ch1_application1 <- ch1_application1 %>%
  filter(exporter != importer)

# ols ----

form <- log_trade ~ log_dist + cntg + lang + clny + log_y + log_e

d <- ch1_application1 %>% filter(trade > 0)

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

# ols remoteness ----

form <- log_trade ~ log_dist + cntg + lang + clny + log_y + log_e +
  log_remoteness_exp + log_remoteness_imp

d <- ch1_application1 %>% filter(trade > 0)

bench_ols_remoteness <- mark(
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

# fe ----

form <- log_trade ~ log_dist + cntg + lang + clny + exp_year + imp_year

d <- ch1_application1 %>% filter(trade > 0)

bench_fe <- mark(
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

form <- trade ~ log_dist + cntg + lang + clny + exp_year + imp_year

d <- ch1_application1

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

rm(d, form, ch1_application1)

save.image("benchmarks/01-chapter1-traditional-gravity-bench.RData", compress = "xz")

rm(list = ls())
gc()
